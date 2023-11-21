{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

module Main where

import Protolude (
  Bool (..),
  Either (Left, Right),
  Eq,
  Generic,
  IO,
  Int,
  Integer,
  Maybe (..),
  Proxy (Proxy),
  Show,
  Text,
  elem,
  encodeUtf8,
  find,
  fromMaybe,
  lastMay,
  mapM,
  mapM_,
  print,
  pure,
  putErrText,
  putText,
  readMaybe,
  show,
  when,
  ($),
  (&),
  (.),
  (<&>),
  (<>),
  (==),
  (>>=),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (
  FromJSON (parseJSON),
  Value,
  eitherDecode,
  encode,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Base (String)
import GitHub qualified as GH
import GitHub.Endpoints.Activity.Starring as GH (Repo, untagName)
import GitHub.Internal.Prelude (fromString)
import Network.HTTP.Client (
  RequestBody (RequestBodyLBS),
  Response (responseHeaders),
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
import Network.HTTP.Link (href, parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link, LinkParam (..), linkParams)
import Network.URI (URI)
import System.Environment (lookupEnv)
import Text.RawString.QQ (r)

import Utils (emptyOwner, emptyRepo, mapMSequentially)

-- | Replaces a variable in a string with a value
var :: Text -> Text -> Text -> Text
var idName =
  T.replace ("<<" <> idName <> ">>")

data SaveStrategy = OverwriteRepo | AddRepo
  deriving (Show, Eq)

data ExtendedRepo = ExtendedRepo
  { core :: GH.Repo
  , commitsCount :: Maybe Integer
  }
  deriving (Show, Eq)

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

loadGitHubToken :: IO (Maybe Text)
loadGitHubToken =
  lookupEnv "GITHUB_TOKEN" <&> (<&> T.pack)

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
      <> ( "commits_count: "
            <> show (extendedRepo & commitsCount & fromMaybe 0)
            <> "\n"
         )
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
getNumberOfCommits :: Repo -> IO (Maybe Integer)
getNumberOfCommits repo = do
  let repoSlug =
        (repo & GH.repoOwner & GH.simpleOwnerLogin & untagName)
          <> "/"
          <> (repo & GH.repoName & untagName)

  putText $ "⏳ Get number of commits for repo " <> repoSlug

  let apiEndpoint =
        "https://api.github.com/repos/" <> repoSlug <> "/commits?per_page=1"

  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack apiEndpoint
  let request =
        initialRequest
          { method = "HEAD"
          , requestHeaders = [("User-Agent", "repos-uploader")]
          }

  response <- httpLbs request manager

  putText $ "✅ Got number of commits for repo " <> repoSlug <> ":"
  putText $ show response.responseStatus
  putText $ show response.responseHeaders

  getLastUrl response
    <&> (show >>> T.pack >>> T.splitOn "&page=")
    >>= lastMay
    >>= readMaybe
    & pure

deleteRepoQuery :: ExtendedRepo -> Text
deleteRepoQuery extendedRepo =
  let
    repo = extendedRepo.core
   in
    [r|
      mutation DeleteRepo {
        delete_repos(
          filter: {
            github_id: { eq: <<github_id>> }
          }
        ) {
          affected_rows
        }
      }
    |]
      & var "github_id" (repo & GH.repoId & GH.untagId & show)

-- | TODO: Airsequel's GraphQL API doesn't support userting yet
upsertRepoQuery :: Text -> ExtendedRepo -> Text
upsertRepoQuery utc extendedRepo =
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
        upsert_repos(
          filter: {
            github_id: { eq: <<github_id>> },
          }
          set: {
            github_id: <<github_id>>
            owner: "<<owner>>"
            name: "<<name>>"
            description: "<<description>>"
            homepage: "<<homepage>>"
            language: "<<language>>"
            stargazers_count: <<stargazers_count>>
            open_issues_count: <<open_issues_count>>
            commits_count: <<commits_count>>
            created_utc: "<<created_utc>>"
            updated_utc: "<<updated_utc>>"
            crawled_utc: "<<crawled_utc>>"
          }
        ) {
            affected_rows
        }
      }
    |]
      & var "github_id" (repo & GH.repoId & GH.untagId & show)
      & var "owner" (repo & GH.repoOwner & GH.simpleOwnerLogin & untagName)
      & var "name" (repo & GH.repoName & untagName)
      & var "description" (repo & GH.repoDescription & fromMaybe "")
      & var "homepage" (repo & GH.repoHomepage & fromMaybe "")
      & var
        "language"
        (repo & GH.repoLanguage <&> GH.getLanguage & fromMaybe "")
      & var "stargazers_count" (repo & GH.repoWatchersCount & show)
      & var "open_issues_count" (repo & GH.repoOpenIssuesCount & show)
      & var "commits_count" (commitsCount & fromMaybe 0 & show)
      & var "created_utc" (getTimestamp GH.repoCreatedAt)
      & var "updated_utc" (getTimestamp GH.repoUpdatedAt)
      & var "crawled_utc" utc

-- | TODO - Also store archived status
insertRepoQuery :: Text -> ExtendedRepo -> Text
insertRepoQuery utc extendedRepo =
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
      mutation InsertRepo {
        insert_repos(objects: [
          {
            github_id: <<github_id>>
            owner: "<<owner>>"
            name: "<<name>>"
            description: "<<description>>"
            homepage: "<<homepage>>"
            language: "<<language>>"
            stargazers_count: <<stargazers_count>>
            open_issues_count: <<open_issues_count>>
            commits_count: <<commits_count>>
            created_utc: "<<created_utc>>"
            updated_utc: "<<updated_utc>>"
            crawled_utc: "<<crawled_utc>>"
          }
        ]) {
          affected_rows
        }
      }
    |]
      & var "github_id" (repo & GH.repoId & GH.untagId & show)
      & var "owner" (repo & GH.repoOwner & GH.simpleOwnerLogin & untagName)
      & var "name" (repo & GH.repoName & untagName)
      & var "description" (repo & GH.repoDescription & fromMaybe "")
      & var "homepage" (repo & GH.repoHomepage & fromMaybe "")
      & var
        "language"
        (repo & GH.repoLanguage <&> GH.getLanguage & fromMaybe "")
      & var "stargazers_count" (repo & GH.repoWatchersCount & show)
      & var "open_issues_count" (repo & GH.repoOpenIssuesCount & show)
      & var "commits_count" (commitsCount & fromMaybe 0 & show)
      & var "created_utc" (getTimestamp GH.repoCreatedAt)
      & var "updated_utc" (getTimestamp GH.repoUpdatedAt)
      & var "crawled_utc" utc

-- | Save the repo in Airsequel via  a POST request executed by http-client
saveRepoInAirsequel :: SaveStrategy -> ExtendedRepo -> IO ()
saveRepoInAirsequel saveStrategy extendedRepo = do
  dbEndpoint <- loadDbEndpoint
  writeToken <- loadWriteToken

  manager <- newManager tlsManagerSettings

  now <- getCurrentTime <&> (iso8601Show >>> T.pack)

  -- Delete the repo first before inserting it
  -- if the save strategy is to overwrite
  when (saveStrategy == OverwriteRepo) $ do
    initialDeleteRequest <- parseRequest $ T.unpack dbEndpoint
    let deleteRequest =
          initialDeleteRequest
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer " <> writeToken & encodeUtf8)
                ]
            , requestBody =
                RequestBodyLBS
                  $ encode
                  $ object ["query" .= deleteRepoQuery extendedRepo]
            }
    deleteResponse <- httpLbs deleteRequest manager
    print deleteResponse

  initialInsertRequest <- parseRequest $ T.unpack dbEndpoint
  let insertRequest =
        initialInsertRequest
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> writeToken & encodeUtf8)
              ]
          , requestBody =
              RequestBodyLBS
                $ encode
                $ object ["query" .= insertRepoQuery now extendedRepo]
          }
  insertResponse <- httpLbs insertRequest manager
  print insertResponse

{- | Loads a single repo from GitHub, adds number of commits,
| and saves it to Airsequel
-}
loadAndSaveRepo :: SaveStrategy -> Text -> Text -> IO ()
loadAndSaveRepo saveStrategy owner name = do
  repoResult <-
    GH.github'
      GH.repositoryR
      (fromString $ T.unpack owner)
      (fromString $ T.unpack name)

  case repoResult of
    Left error -> putErrText $ "Error: " <> show error
    Right repo -> do
      commitsCount <- getNumberOfCommits repo
      let extendedRepo =
            ExtendedRepo
              { core = repo
              , commitsCount = commitsCount
              }
      putText $ formatRepo extendedRepo
      saveRepoInAirsequel saveStrategy extendedRepo

data RepoObject = RepoObject
  { owner :: Text
  , name :: Text
  , githubId :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON RepoObject where
  parseJSON = withObject "RepoObject" $ \o -> do
    ownerObj <- o .: "owner"
    owner <- ownerObj .: "login"
    name <- o .: "name"
    githubId <- o .: "databaseId"
    pure RepoObject{owner, name, githubId}

data GqlResponse = GqlResponse
  { repos :: [RepoObject]
  , errorsMb :: Maybe Value
  , nextCursorMb :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GqlResponse where
  parseJSON = withObject "GqlResponse" $ \o -> do
    data_ <- o .: "data"
    errorsMb <- o .:? "errors"
    search <- data_ .: "search"
    edges <- search .: "edges"
    repos <- edges & mapM (.: "node")

    pageInfo <- search .: "pageInfo"
    nextCursorMb <- pageInfo .:? "endCursor"

    pure GqlResponse{repos, errorsMb, nextCursorMb}

execGqlQuery ::
  Text ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  [ExtendedRepo] ->
  IO [ExtendedRepo]
execGqlQuery apiEndpoint tokenMb query nextCursorMb initialRepos = do
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest $ T.unpack apiEndpoint
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders =
              [ ("User-Agent", "repos-uploader")
              , ("Content-Type", "application/json")
              ]
                <> case tokenMb of
                  Just token ->
                    [("Authorization", "Bearer " <> token & encodeUtf8)]
                  Nothing -> []
          , requestBody =
              RequestBodyLBS
                $ encode
                $ object
                  [ "query"
                      .= ( query
                            & var
                              "optionalAfter"
                              ( nextCursorMb
                                  <&> (\cursor -> "after: \"" <> cursor <> "\"")
                                  & fromMaybe ""
                              )
                         )
                  ]
          }
  response <- httpLbs request manager
  let gqlResult :: Either String GqlResponse =
        response & responseBody & eitherDecode

  case gqlResult of
    Left error -> do
      putErrText $ "HTTP Error: " <> show error
      pure []
    Right gqlResponse -> do
      case gqlResponse.errorsMb of
        Just errors -> putErrText $ "GraphQL Errors:\n" <> show errors
        Nothing -> pure ()

      let repos :: [GH.Repo] =
            gqlResponse.repos <&> \repoObj ->
              emptyRepo
                { GH.repoOwner =
                    emptyOwner
                      { GH.simpleOwnerLogin =
                          GH.mkOwnerName repoObj.owner
                      }
                , GH.repoName = GH.mkRepoName repoObj.name
                , GH.repoId =
                    GH.mkId
                      (Proxy :: Proxy GH.Repo)
                      repoObj.githubId
                }

      commitsCounts <- mapMSequentially 1000 getNumberOfCommits repos

      let extendedRepos =
            P.zipWith
              ( \repo commitsCount ->
                  ExtendedRepo
                    { core = repo
                    , commitsCount
                    }
              )
              repos
              commitsCounts

      case gqlResponse.nextCursorMb of
        Nothing -> pure $ initialRepos <> extendedRepos
        Just nextCursor -> do
          execGqlQuery
            apiEndpoint
            tokenMb
            query
            (Just nextCursor)
            (initialRepos <> extendedRepos)

getReposViaSearch :: Maybe Text -> Text -> IO [ExtendedRepo]
getReposViaSearch githubToken searchQuery = do
  let gqlQUery =
        [r|
          {
            search(
              query: "<<searchQuery>>",
              type: REPOSITORY,
              first: 100
              <<optionalAfter>>
            ) {
              edges {
                node {
                  ... on Repository {
                    owner { login }
                    name
                    databaseId
                  }
                }
              }
              pageInfo {
                endCursor
              }
            }
          }
        |]
          & var "searchQuery" searchQuery

  execGqlQuery
    "https://api.github.com/graphql"
    githubToken
    gqlQUery
    Nothing
    []

main :: IO ()
main = do
  githubTokenMb <- loadGitHubToken

  -- TODO: Add CLI flag to load and save a single repo
  -- loadAndSaveRepo OverwriteRepo "Airsequel" "SQLiteDAV"

  repos <-
    getReposViaSearch
      githubTokenMb
      "language:haskell stars:>500 sort:stars-desc"

  putText $ "Found " <> show (P.length repos) <> " repos:"
  repos
    <&> ( \repo ->
            (repo.core.repoOwner.simpleOwnerLogin & untagName)
              <> ("/" :: Text)
              <> (repo.core.repoName & untagName)
              <> (" " :: Text)
              <> show repo.commitsCount
        )
    & mapM_ putText

  putText $ "⏳ Save " <> show (P.length repos) <> " repos to Airsequel …"
  -- TODO: Add CLI flag to choose between OverwriteRepo and AddRepo
  repos & mapM_ (saveRepoInAirsequel OverwriteRepo)

  pure ()
