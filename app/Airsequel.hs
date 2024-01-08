{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use fmap" #-}

module Airsequel where

import Protolude (
  Either (..),
  IO,
  Integer,
  Maybe (..),
  Text,
  encodeUtf8,
  filter,
  fromMaybe,
  isJust,
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

import Numeric (showInt)
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
    mutation InsertRepo( $objects: [repos_insert_input!]! ) {
      insert_repos(
        objects: $objects
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


-- | Load Airsequel rowid of repos by their GitHub ID
loadRowids :: Manager -> Text -> Text -> [Repo] -> IO [Repo]
loadRowids manager dbEndpoint airseqWriteToken repos = do
  let
    getReposWithRowidQuery :: Text
    getReposWithRowidQuery =
      [r|
        query GetRowids ($githubIds: [Int]) {
          repos(filter: { github_id: { in: $githubIds } }) {
            databaseId: github_id
            rowid
          }
        }
      |]

  initialGetRowidsRequest <- parseRequest $ T.unpack dbEndpoint

  let getRowidsRequest =
        setRequestFields
          airseqWriteToken
          getReposWithRowidQuery
          (KeyMap.fromList ["githubIds" .= (repos <&> githubId)])
          initialGetRowidsRequest

  getReposWithRowidResponse <- httpLbs getRowidsRequest manager

  when
    (getReposWithRowidResponse.responseStatus.statusCode /= 200)
    (putErrText $ show getReposWithRowidResponse.responseBody)

  let
    ghIdsWithRowid
      :: Either [P.Char] [(Integer {- githubId -}, Integer {- rowid -})] =
        (getReposWithRowidResponse.responseBody & eitherDecode)
          >>= ( \gqlRes ->
                  P.flip parseEither gqlRes $ \gqlResObj -> do
                    gqlData <- gqlResObj .: "data"
                    gqlData .: "repos"
              )
            <&> ( \(reposWithRowid :: [Repo]) ->
                    reposWithRowid
                      & filter (\repoWithRowid -> isJust repoWithRowid.rowid)
                      <&> ( \repoWithRowid ->
                              ( repoWithRowid.githubId
                              , repoWithRowid.rowid & fromMaybe 0
                              )
                          )
                )

  case ghIdsWithRowid of
    Left err -> do
      putErrLn err
      pure repos
    Right rowids -> do
      P.putStrLn $
        "\nℹ️ "
          <> showInt (P.length rowids) " of "
          <> showInt (P.length repos) " repos already exist in Airsequel"

      pure $
        repos <&> \repo ->
          repo
            { rowid =
                rowids
                  & filter (\(ghId, _) -> ghId == repo.githubId)
                  & P.head
                  <&> P.snd
            }


{-| Insert or upsert repos in Airsequel
via a POST request executed by http-client
-}
saveReposInAirsequel :: SaveStrategy -> [Repo] -> IO ()
saveReposInAirsequel saveStrategy repos = do
  P.putText $ "\n⏳ Saving " <> show (P.length repos) <> " repos in Airsequel …"

  dbEndpoint <- loadDbEndpoint
  airseqWriteToken <- loadAirsWriteToken

  manager <- newManager tlsManagerSettings

  now <- getCurrentTime <&> (iso8601Show >>> T.pack)

  -- Get rowid for repos if repos shall be overwritten
  reposNorm <- case saveStrategy of
    OverwriteRepo -> loadRowids manager dbEndpoint airseqWriteToken repos
    AddRepo -> pure repos

  initialInsertRequest <- parseRequest $ T.unpack dbEndpoint

  let
    objects =
      reposNorm <&> \repo ->
        object
          [ "rowid" .= repo.rowid
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
          , "created_utc" .= (repo.createdAt <&> iso8601Show)
          , "updated_utc" .= (repo.updatedAt <&> iso8601Show)
          , "crawled_utc" .= now
          ]

    insertRequest =
      setRequestFields
        airseqWriteToken
        upsertRepoQuery
        (KeyMap.fromList ["objects" .= objects])
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
