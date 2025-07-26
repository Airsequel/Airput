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
  Int,
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
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (
  Object,
  Value (Object),
  eitherDecode,
  encode,
  object,
  (.:),
  (.=),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
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

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromText)
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
upsertRepoQuery :: Text -> Text
upsertRepoQuery tableName =
  [r|
    mutation InsertRepo( $objects: [TABLE_NAME_insert_input!]! ) {
      insert_TABLE_NAME(
        objects: $objects
        on_conflict: {
          constraint: rowid
          update_columns: [
            description
            homepage
            language
            stargazers_count
            open_issues_count
            open_prs_count
            commits_count
            is_private
            is_archived
            updated_utc
            pushed_utc
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
    & T.replace "TABLE_NAME" tableName


-- | Load Airsequel rowid of repos by their GitHub ID
loadRowids :: Manager -> Text -> Text -> Text -> [Repo] -> IO [Repo]
loadRowids manager dbEndpoint airseqWriteToken tableName repos = do
  -- Build the query with dynamic table name
  let
    getReposWithRowidQuery :: Text
    getReposWithRowidQuery =
      [r|
        query GetRowids ($githubIds: [Int]) {
          TABLE_NAME(filter: { github_id: { in: $githubIds } }) {
            databaseId: github_id
            rowid
          }
        }
      |]
        & T.replace "TABLE_NAME" tableName

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

  -- Check for GraphQL errors
  let gqlRes :: Either String GqlRes =
        getReposWithRowidResponse.responseBody
          & eitherDecode

  case gqlRes of
    Left err -> do
      putErrText "Error parsing GraphQL response:"
      putErrLn err
      pure repos
    Right GqlRes{gqlErrors, gqlData} ->
      case gqlErrors of
        Just errs -> do
          putErrText "GraphQL errors:"
          errs
            <&> encodePretty
            & P.mapM_ P.putLByteString
          pure repos
        Nothing -> do
          let
            repoParser :: Maybe Value -> Parser [Repo]
            repoParser = \case
              Just (Object obj) -> obj .: fromText tableName
              _ -> P.mempty

            -- \| … [(githubId, rowid)]
            ghIdsWithRowid :: Either [P.Char] [(Integer, Integer)] =
              gqlData
                & parseEither repoParser
                <&> ( \(reposWithRowid :: [Repo]) ->
                        reposWithRowid
                          & filter
                            ( \repoWithRowid ->
                                isJust repoWithRowid.rowid
                            )
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
              P.putText $
                "\nℹ️ "
                  <> show @Int (P.length rowids)
                  <> " of "
                  <> show @Int (P.length repos)
                  <> " repos already exist in Airsequel"

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
  P.putText $
    "\n⏳ Saving " <> show @Int (P.length repos) <> " repos in Airsequel …"

  dbEndpoint <- loadDbEndpoint
  airseqWriteToken <- loadAirsWriteToken

  manager <- newManager tlsManagerSettings

  now <- getCurrentTime <&> (iso8601Show >>> T.pack)

  let tableName = case saveStrategy of
        OverwriteRepo tn -> tn
        AddRepo tn -> tn

  -- Get rowid for repos if repos shall be overwritten
  reposNorm <- case saveStrategy of
    OverwriteRepo tn -> loadRowids manager dbEndpoint airseqWriteToken tn repos
    AddRepo _ -> pure repos

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
          , "open_prs_count" .= repo.openPrsCount
          , "commits_count" .= (repo.commitsCount & fromMaybe 0)
          , "is_private" .= repo.isPrivate
          , "is_archived" .= repo.isArchived
          , "created_utc" .= (repo.createdAt <&> iso8601Show)
          , "updated_utc" .= (repo.updatedAt <&> iso8601Show)
          , "pushed_utc" .= (repo.pushedAt <&> iso8601Show)
          , "crawled_utc" .= now
          ]

    insertRequest =
      setRequestFields
        airseqWriteToken
        (upsertRepoQuery tableName)
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
      putErrLn err
    Right GqlRes{gqlErrors} ->
      case gqlErrors of
        Nothing -> pure ()
        Just errs -> do
          putErrText "GraphQL errors:"
          putErrText $ show errs


-- | Delete the repo from Airsequel
deleteRepo :: Manager -> Text -> Text -> Repo -> Text -> IO ()
deleteRepo manager dbEndpoint airseqWriteToken repo tableName = do
  let
    deleteRepoQuery :: Text
    deleteRepoQuery =
      [r|
        mutation DeleteRepo($github_id: Int!) {
          delete_TABLE_NAME(
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
        & T.replace "TABLE_NAME" tableName

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
