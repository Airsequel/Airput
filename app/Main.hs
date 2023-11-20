{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

module Main where

import Protolude as P (
  Bool,
  Either (Left, Right),
  IO,
  Integer,
  Maybe (..),
  Text,
  elem,
  encodeUtf8,
  find,
  fromMaybe,
  lastMay,
  print,
  pure,
  putErrText,
  putLByteString,
  putText,
  readMaybe,
  show,
  ($),
  (&),
  (.),
  (<&>),
  (<>),
  (>>=),
 )

import Data.Aeson (encode, object, (.=))
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Client (
  RequestBody (RequestBodyLBS),
  Response (responseHeaders),
  httpLbs,
  method,
  newManager,
  parseRequest,
  requestBody,
  requestHeaders,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Link.Types (Link, LinkParam (..), linkParams)
import Network.URI (URI)
import Text.RawString.QQ (r)

import Control.Arrow ((>>>))
import Data.List (lookup)
import GitHub qualified as GH
import GitHub.Endpoints.Activity.Starring as GH (Repo, untagName)
import Network.HTTP.Link (href, parseLinkHeaderBS)
import System.Environment (lookupEnv)

data ExtendedRepo = ExtendedRepo
  { core :: GH.Repo
  , commitsCount :: Integer
  }

-- | The ID of the Airsequel database loaded from the environment
loadDbId :: IO Text
loadDbId =
  lookupEnv "AIRSEQUEL_DB_ID" <&> (fromMaybe "" >>> T.pack)

loadDbEndpoint :: IO Text
loadDbEndpoint = do
  dbId <- loadDbId
  pure $ "https://www.airsequel.com/dbs/" <> dbId <> "/graphql"

loadWriteToken :: IO Text
loadWriteToken =
  lookupEnv "AIRSEQUEL_API_TOKEN" <&> (fromMaybe "" >>> T.pack)

formatRepo :: ExtendedRepo -> Text
formatRepo extendedRepo =
  let
    repo = core extendedRepo
   in
    "\n\n"
      <> ("repo_url: " <> show (GH.repoHtmlUrl repo) <> "\n")
      <> ( "description: "
            <> (repo & GH.repoDescription & fromMaybe "")
            <> "\n"
         )
      <> ("homepage: " <> (repo & GH.repoHomepage & fromMaybe "") <> "\n")
      <> ( "language: "
            <> (repo & GH.repoLanguage <&> GH.getLanguage & fromMaybe "")
            <> "\n"
         )
      <> ("stargazers_count: " <> show (GH.repoStargazersCount repo) <> "\n")
      <> ("commits_count: " <> show (commitsCount extendedRepo) <> "\n")
      <> ("open_issues_count: " <> show (GH.repoOpenIssuesCount repo) <> "\n")
      <> ( "created_at: "
            <> (repo & GH.repoCreatedAt <&> show & fromMaybe "")
            <> "\n"
         )
      <> ( "updated_at: "
            <> (repo & GH.repoUpdatedAt <&> show & fromMaybe "")
            <> "\n"
         )

-- queryRepos :: Text
-- queryRepos =
--   [r|
--     query reposQuery {
--       repos( limit: 100 ) {
--         rowid
--         id
--         name
--         language
--         url
--         stars
--         updated_utc
--       }
--     }
--   |]

-- | Query @Link@ header with @rel=last@ from the request headers
getLastUrl :: Response a -> Maybe URI
getLastUrl req = do
  let
    isRelNext :: Link uri -> Bool
    isRelNext = elem relNextLinkParam . linkParams

    relNextLinkParam :: (LinkParam, Text)
    relNextLinkParam = (Rel, "last")

  linkHeader <- lookup "Link" (responseHeaders req)
  links <- parseLinkHeaderBS linkHeader
  nextURI <- find isRelNext links
  pure $ href nextURI

{- | Workaround to get the number of commits for a repo
| https://stackoverflow.com/a/70610670
-}
getNumberOfCommits :: Repo -> IO Integer
getNumberOfCommits repo = do
  let apiEndpoint =
        "https://api.github.com/repos/"
          <> (repo & GH.repoOwner & GH.simpleOwnerLogin & untagName)
          <> "/"
          <> (repo & GH.repoName & untagName)
          <> "/commits?per_page=1"

  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack apiEndpoint
  let request =
        initialRequest
          { method = "HEAD"
          , requestHeaders = [("User-Agent", "repos-uploader")]
          }

  response <- httpLbs request manager

  getLastUrl response
    <&> (show >>> T.pack >>> T.splitOn "&page=")
    >>= lastMay
    >>= readMaybe
    & fromMaybe 0
    & pure

insertRepoQuery :: ExtendedRepo -> Text
insertRepoQuery extendedRepo =
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
        insert_repos(objects: [
          {
            owner: "{{owner}}"
            name: "{{name}}"
            description: "{{description}}"
            homepage: "{{homepage}}"
            language: "{{language}}"
            stargazers_count: {{stargazers_count}}
            open_issues_count: {{open_issues_count}}
            commits_count: {{commits_count}}
            created_utc: "{{created_utc}}"
            updated_utc: "{{updated_utc}}"
          }
        ]) {
          affected_rows
        }
      }
    |]
      & T.replace
        "{{owner}}"
        (repo & GH.repoOwner & GH.simpleOwnerLogin & untagName)
      & T.replace "{{name}}" (repo & GH.repoName & untagName)
      & T.replace
        "{{description}}"
        (repo & GH.repoDescription & fromMaybe "")
      & T.replace "{{homepage}}" (repo & GH.repoHomepage & fromMaybe "")
      & T.replace
        "{{language}}"
        (repo & GH.repoLanguage <&> GH.getLanguage & fromMaybe "")
      & T.replace "{{stargazers_count}}" (repo & GH.repoWatchersCount & show)
      & T.replace
        "{{open_issues_count}}"
        (repo & GH.repoOpenIssuesCount & show)
      & T.replace "{{commits_count}}" (show commitsCount)
      & T.replace "{{created_utc}}" (getTimestamp GH.repoCreatedAt)
      & T.replace "{{updated_utc}}" (getTimestamp GH.repoUpdatedAt)

-- | Save the repo in Airsequel via  a POST request executed by http-client
saveRepoInAirsequel :: ExtendedRepo -> IO ()
saveRepoInAirsequel extendedRepo = do
  dbEndpoint <- loadDbEndpoint
  writeToken <- loadWriteToken

  manager <- newManager tlsManagerSettings

  let requestObject = object ["query" .= insertRepoQuery extendedRepo]
  initialRequest <- parseRequest $ T.unpack dbEndpoint
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> writeToken & encodeUtf8)
              ]
          , requestBody = RequestBodyLBS $ encode requestObject
          }

  putLByteString $ encode requestObject

  response <- httpLbs request manager
  print response

main :: IO ()
main = do
  possibleRepo <- GH.github () GH.repositoryR "Airsequel" "SQLiteDAV"
  case possibleRepo of
    Left error ->
      putErrText $ "Error: " <> show error
    Right repo -> do
      commitsCount <- getNumberOfCommits repo
      let extendedRepo =
            ExtendedRepo
              { core = repo
              , commitsCount = commitsCount
              }
      putText $ formatRepo extendedRepo
      saveRepoInAirsequel extendedRepo
