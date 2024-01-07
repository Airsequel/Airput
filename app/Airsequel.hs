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
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Base (String)
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

import Types (GqlRes (..), Repo (..), SaveStrategy (..))
import Utils (loadAirsWriteToken, loadDbEndpoint)


setRequestFields :: Text -> Text -> Object -> Request -> Request
setRequestFields airseqWriteToken query variables req =
  req
    { method = "POST"
    , requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Authorization", "Bearer " <> airseqWriteToken & encodeUtf8)
        ]
    , requestBody =
        RequestBodyLBS $
          encode $
            object
              [ "query" .= query
              , "variables" .= variables
              ]
    }


-- | Insert repos in Airsequel or update them if rowid is already assigned
upsertRepoQuery :: Text
upsertRepoQuery = do
  [r|
    mutation InsertRepo (
      $rowid: Int
      $github_id: Int!
      $owner: String!
      $name: String!
      $description: String
      $homepage: String
      $language: String
      $stargazers_count: Int
      $open_issues_count: Int
      $commits_count: Int
      $is_archived: Boolean
      $created_utc: String
      $updated_utc: String
      $crawled_utc: String!
    ) {
      insert_repos(
        objects: [{
          rowid: $rowid
          github_id: $github_id
          owner: $owner
          name: $name
          description: $description
          homepage: $homepage
          language: $language
          stargazers_count: $stargazers_count
          open_issues_count: $open_issues_count
          commits_count: $commits_count
          is_archived: $is_archived
          created_utc: $created_utc
          updated_utc: $updated_utc
          crawled_utc: $crawled_utc
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


-- | Get rowid of a repo with the specified GitHub ID
getRowid :: Manager -> Text -> Text -> Repo -> IO (Maybe Int)
getRowid manager dbEndpoint airseqWriteToken repo = do
  let
    githubId = repo.githubId

    getRowidQuery :: Text
    getRowidQuery =
      [r|
        query GetRowid($github_id: Int!) {
          repos(
            filter: { github_id: { eq: $github_id } }
          ) {
            rowid
          }
        }
      |]

  initialGetRowidRequest <- parseRequest $ T.unpack dbEndpoint

  let getRowidRequest =
        setRequestFields
          airseqWriteToken
          getRowidQuery
          (KeyMap.fromList ["github_id" .= githubId])
          initialGetRowidRequest
  getRowidResponse <- httpLbs getRowidRequest manager

  when
    (getRowidResponse.responseStatus.statusCode /= 200)
    (putErrText $ show getRowidResponse.responseBody)

  let
    repoSlug = repo.owner <> "/" <> repo.name
    msgBase =
      "Repo \"" <> repoSlug <> "\" is not"

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
                [repoObj :: Object] -> parseEither (.: "rowid") repoObj
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
        "Repo \""
          <> repoSlug
          <> "\" is already in Airsequel "
          <> ("(rowid " <> show rowid <> ") ")
          <> "and will be updated."
      pure $ Just rowid


{-| Insert or upsert the repo in Airsequel
via a POST request executed by http-client
-}
saveRepoInAirsequel :: SaveStrategy -> Repo -> IO ()
saveRepoInAirsequel saveStrategy repo = do
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
          repo
      else pure Nothing

  initialInsertRequest <- parseRequest $ T.unpack dbEndpoint

  let
    variables =
      [ "rowid" .= rowidMb
      , "github_id" .= repo.githubId
      , "owner" .= repo.owner
      , "name" .= repo.name
      , "description" .= repo.description
      , "homepage" .= repo.homepageUrl
      , "language" .= repo.primaryLanguage
      , "stargazers_count" .= repo.stargazerCount
      , "open_issues_count" .= repo.openIssuesCount
      , "commits_count" .= (repo.commitsCount & fromMaybe 0)
      , "is_archived" .= repo.isArchived
      , "created_utc" .= iso8601Show repo.createdAt
      , "updated_utc" .= iso8601Show repo.updatedAt
      , "crawled_utc" .= now
      ]

    insertRequest =
      setRequestFields
        airseqWriteToken
        upsertRepoQuery
        (KeyMap.fromList variables)
        initialInsertRequest

  insertResponse <- httpLbs insertRequest manager

  when (insertResponse.responseStatus.statusCode /= 200) $ do
    putErrText "Http Error:"
    putErrText $ show insertResponse.responseBody

  -- Check for GraphQL errors
  let gqlRes :: Either String GqlRes =
        insertResponse.responseBody & eitherDecode

  case gqlRes of
    Left err -> do
      putErrText "Error parsing GraphQL response:"
      P.putStrLn err
    Right GqlRes{gqlErrors} ->
      case gqlErrors of
        Nothing -> pure ()
        Just errs -> do
          putErrText "GraphQL errors:"
          putErrText $ show errs


-- | Delete the repo from Airsequel
deleteRepo :: Manager -> Text -> Text -> Repo -> IO ()
deleteRepo manager dbEndpoint airseqWriteToken repo = do
  let
    deleteRepoQuery :: Text
    deleteRepoQuery =
      [r|
        mutation DeleteRepo($github_id: Int!) {
          delete_repos(
            filter: {
              github_id: { eq: $github_id }
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

  initialDeleteRequest <- parseRequest $ T.unpack dbEndpoint

  let deleteRequest =
        setRequestFields
          airseqWriteToken
          deleteRepoQuery
          ( KeyMap.fromList
              ["github_id" .= repo.githubId]
          )
          initialDeleteRequest

  deleteResponse <- httpLbs deleteRequest manager
  print deleteResponse
