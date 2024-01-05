{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
{-# HLINT ignore "Use unless" #-}

module Airsequel where

import Protolude (
  Either (..),
  IO,
  Int,
  Maybe (..),
  Text,
  encodeUtf8,
  fromMaybe,
  print,
  pure,
  putErrLn,
  putErrText,
  show,
  when,
  ($),
  (&),
  (/=),
  (<&>),
  (<>),
  (==),
  (>>=),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (
  Object,
  eitherDecode,
  encode,
  object,
  (.:),
  (.=),
 )
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GitHub qualified as GH
import GitHub.Endpoints.Activity.Starring as GH (untagName)
import Network.HTTP.Client (
  Manager,
  Request,
  RequestBody (RequestBodyLBS),
  httpLbs,
  method,
  newManager,
  parseRequest,
  requestBody,
  requestHeaders,
  responseBody,
  responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import Text.RawString.QQ (r)

import Data.Aeson.Types (parseEither)
import Types (ExtendedRepo (..), SaveStrategy (..))
import Utils (encodeToText, loadAirsWriteToken, loadDbEndpoint, var)


setRequestFields :: Text -> Text -> Request -> Request
setRequestFields airseqWriteToken query req =
  req
    { method = "POST"
    , requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Authorization", "Bearer " <> airseqWriteToken & encodeUtf8)
        ]
    , requestBody =
        RequestBodyLBS $
          encode $
            object ["query" .= query]
    }


-- | Insert repos in Airsequel and update them if rowid is already assigned
upsertRepoQuery :: Text -> ExtendedRepo -> Maybe Int -> Text
upsertRepoQuery utc extendedRepo rowidMb =
  let
    repo = extendedRepo.core
    commitsCount = extendedRepo.commitsCount
    getTimestamp field =
      repo
        & field
        <&> iso8601Show
        & fromMaybe ""
        & T.pack
  in
    [r|
      mutation {
        insert_repos(
          objects: [{
            <<rowid>>
            github_id: <<github_id>>
            owner: "<<owner>>"
            name: "<<name>>"
            description: <<description>>
            homepage: "<<homepage>>"
            language: "<<language>>"
            stargazers_count: <<stargazers_count>>
            open_issues_count: <<open_issues_count>>
            <<commits_count>>
            is_archived: <<is_archived>>
            created_utc: "<<created_utc>>"
            updated_utc: "<<updated_utc>>"
            crawled_utc: "<<crawled_utc>>"
          }]
          on_conflict: {
            constraint: rowid
            update_columns: [
              description
              homepage
              language
              stargazers_count
              open_issues_count
              commits_count
              is_archived
              updated_utc
              crawled_utc
            ]
          }
        ) {
            affected_rows
            returning {
              owner
              name
              rowid
            }
        }
      }
    |]
      & var
        "rowid"
        ( case rowidMb of
            Just rowid -> "rowid: " <> show rowid
            Nothing -> ""
        )
      & var "github_id" (repo.repoId & GH.untagId & show)
      & var "owner" (repo.repoOwner.simpleOwnerLogin & untagName)
      & var "name" (repo.repoName & untagName)
      & var "description" (repo.repoDescription & fromMaybe "" & encodeToText)
      & var "homepage" (repo.repoHomepage & fromMaybe "")
      & var
        "language"
        (repo.repoLanguage <&> GH.getLanguage & fromMaybe "")
      & var "stargazers_count" (repo.repoStargazersCount & show)
      & var "open_issues_count" (repo.repoOpenIssuesCount & show)
      & var
        "commits_count"
        ( case commitsCount of
            Just count -> "commits_count: " <> show count
            Nothing -> ""
        )
      & var "is_archived" (repo.repoArchived & show & T.toLower)
      & var "created_utc" (getTimestamp GH.repoCreatedAt)
      & var "updated_utc" (getTimestamp GH.repoUpdatedAt)
      & var "crawled_utc" utc


-- | Get rowid of a repo with the specified GitHub ID
getRowid :: Manager -> Text -> Text -> ExtendedRepo -> IO (Maybe Int)
getRowid manager dbEndpoint airseqWriteToken extendedRepo = do
  let
    githubId = extendedRepo.core.repoId & GH.untagId

    getRowidQuery :: Text
    getRowidQuery =
      [r|
        query GetRowid {
          repos(
            filter: {
              github_id: { eq: <<github_id>> }
            }
          ) {
            rowid
          }
        }
      |]
        & var "github_id" (show githubId)

  initialGetRowidRequest <- parseRequest $ T.unpack dbEndpoint

  let getRowidRequest =
        setRequestFields
          airseqWriteToken
          getRowidQuery
          initialGetRowidRequest
  getRowidResponse <- httpLbs getRowidRequest manager

  when
    (getRowidResponse.responseStatus.statusCode /= 200)
    (putErrText $ show getRowidResponse.responseBody)

  let
    msgBase =
      "Repo \""
        <> (extendedRepo.core.repoOwner.simpleOwnerLogin & untagName)
        <> "/"
        <> (extendedRepo.core.repoName & untagName)
        <> "\" is not"

    rowidResult :: Either [P.Char] Int =
      ( getRowidResponse.responseBody
          & eitherDecode
          :: Either [P.Char] Object
      )
        >>= ( \gqlRes ->
                P.flip parseEither gqlRes $ \gqlResObj -> do
                  gqlData <- gqlResObj .: "data"
                  gqlData .: "repos"
            )
        >>= ( \case
                [] -> Left $ T.unpack $ msgBase <> " in Airsequel yet"
                [repo :: Object] -> parseEither (.: "rowid") repo
                _ ->
                  Left $
                    T.unpack $
                      "Error: " <> msgBase <> " unique in Airsequel"
            )

  case rowidResult of
    Left err -> do
      putErrLn err
      pure Nothing
    Right rowid -> do
      P.putText $
        "Repo is already in Airsequel with rowid \""
          <> show rowid
          <> "\" and will be updated."
      pure $ Just rowid


{-| Insert or upsert the repo in Airsequel
via a POST request executed by http-client
-}
saveRepoInAirsequel :: SaveStrategy -> ExtendedRepo -> IO ()
saveRepoInAirsequel saveStrategy extendedRepo = do
  dbEndpoint <- loadDbEndpoint
  airseqWriteToken <- loadAirsWriteToken

  manager <- newManager tlsManagerSettings

  now <- getCurrentTime <&> (iso8601Show >>> T.pack)

  -- Get rowid for the repo to execute an upsert
  -- if the save strategy is to overwrite
  rowidMb <-
    if saveStrategy == OverwriteRepo
      then
        getRowid
          manager
          dbEndpoint
          airseqWriteToken
          extendedRepo
      else pure Nothing

  initialInsertRequest <- parseRequest $ T.unpack dbEndpoint
  let insertRequest =
        setRequestFields
          airseqWriteToken
          (upsertRepoQuery now extendedRepo rowidMb)
          initialInsertRequest
  insertResponse <- httpLbs insertRequest manager
  print insertResponse


-- | Delete the repo from Airsequel
deleteRepo :: Manager -> Text -> Text -> ExtendedRepo -> IO ()
deleteRepo manager dbEndpoint airseqWriteToken extendedRepo = do
  let
    deleteRepoQuery :: ExtendedRepo -> Text
    deleteRepoQuery extRepo =
      let
        repo = extRepo.core
      in
        [r|
          mutation DeleteRepo {
            delete_repos(
              filter: {
                github_id: { eq: <<github_id>> }
              }
            ) {
              affected_rows
              returning {
                owner
                name
                rowid
              }
            }
          }
        |]
          & var "github_id" (repo.repoId & GH.untagId & show)

  initialDeleteRequest <- parseRequest $ T.unpack dbEndpoint
  let deleteRequest =
        setRequestFields
          airseqWriteToken
          (deleteRepoQuery extendedRepo)
          initialDeleteRequest
  deleteResponse <- httpLbs deleteRequest manager
  print deleteResponse
