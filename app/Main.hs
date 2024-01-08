{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Use maybe" #-}

module Main where

import Protolude (
  Bool (..),
  Either (Left, Right),
  IO,
  Int,
  Maybe (..),
  Text,
  elem,
  encodeUtf8,
  find,
  fromMaybe,
  headMay,
  lastMay,
  mapM_,
  mempty,
  pure,
  putErrText,
  putStrLn,
  putText,
  show,
  when,
  ($),
  (&),
  (.),
  (<$>),
  (<&>),
  (<>),
  (>>=),
 )
import Protolude qualified as P

import Data.Aeson (Value (String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (lookup)
import Data.Text qualified as T
import GHC.Base (String)
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
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Link (href, parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link, LinkParam (..), linkParams)
import Network.URI (URI)
import Options.Applicative (
  Parser,
  argument,
  command,
  execParser,
  fullDesc,
  headerDoc,
  helper,
  hsubparser,
  info,
  metavar,
  progDesc,
  progDescDoc,
  str,
  (<**>),
 )
import Text.RawString.QQ (r)

import Airsequel (saveReposInAirsequel)
import Numeric (showInt)
import Options.Applicative.Help.Pretty (vsep)
import Types (GqlRepoRes (..), Repo (..), SaveStrategy (..))
import Utils (loadGitHubToken)


data CliCmd
  = -- | Upload a single repo
    Upload Text
  | -- | Search for repos and upload them
    Search Text


commands :: Parser CliCmd
commands = do
  let
    upload :: Parser CliCmd
    upload = Upload <$> argument str (metavar "REPO_SLUG")

    search :: Parser CliCmd
    search = Search <$> argument str (metavar "SEARCH_QUERY")

  hsubparser
    ( mempty
        <> command
          "upload"
          (info upload (progDesc "Upload a single repo"))
        <> command
          "search"
          ( info
              search
              ( progDescDoc $
                  Just $
                    vsep
                      [ "Search for and upload several repos."
                      , "WARNING: If the search returns more than 1000 repos,"
                      , "  the results will be truncated."
                      , ""
                      , "Good search options are:"
                      , "- language:haskell"
                      , "- stars:>=10"
                      , "- stars:10..50"
                      , "- sort:updated-desc"
                      , "- sort:stars-asc"
                      , "- archived:true"
                      ]
              )
          )
    )


formatRepo :: Repo -> Text
formatRepo repo = do
  let repoSlug =
        (repo.owner & fromMaybe "")
          <> "/"
          <> (repo.name & fromMaybe "")

  "\n\n"
    <> ("repo_url: github.com/" <> repoSlug <> "\n")
    <> ("description: " <> (repo.description & fromMaybe "") <> "\n")
    <> ("homepage: " <> (repo.homepageUrl & fromMaybe "") <> "\n")
    <> ("language: " <> (repo.primaryLanguage & fromMaybe "") <> "\n")
    <> ("stargazers_count: " <> show repo.stargazerCount <> "\n")
    <> ("commits_count: " <> show (repo.commitsCount & fromMaybe 0) <> "\n")
    <> ("open_issues_count: " <> show repo.openIssuesCount <> "\n")
    <> ("is_archived: " <> (repo.isArchived & show & T.toLower) <> "\n")
    <> ("created_at: " <> show repo.createdAt <> "\n")
    <> ("updated_at: " <> show repo.updatedAt <> "\n")


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


{-| Loads a single repo from GitHub, adds number of commits,
| and saves it to Airsequel
-}
loadAndSaveRepo :: Maybe Text -> SaveStrategy -> Text -> Text -> IO [Repo]
loadAndSaveRepo ghTokenMb _TODO_saveStrategy owner name = do
  let gqlQUery =
        [r|
          query GetSingleRepo($owner: String!, $name: String!) {
            repository(owner: $owner, name: $name) {
              name
              owner {
                login
              }
              databaseId
              stargazerCount
              description
              homepageUrl
              primaryLanguage {
                name
              }
              issues(states: [OPEN]) {
                totalCount
              }
              isArchived
              createdAt
              updatedAt
              defaultBranchRef {
                target {
                  ... on Commit {
                    history {
                      totalCount
                    }
                  }
                }
              }
            }
          }
        |]

  execGithubGqlQuery
    ghTokenMb
    gqlQUery
    ( KeyMap.fromList
        [ "owner" .= owner
        , "name" .= name
        ]
    )
    []


getGhHeaders :: (P.IsString a) => Maybe Text -> [(a, P.ByteString)]
getGhHeaders tokenMb =
  [ ("User-Agent", "repos-uploader")
  , ("Accept", "application/vnd.github+json")
  , ("X-GitHub-Api-Version", "2022-11-28")
  ]
    <> case tokenMb of
      Just token ->
        [("Authorization", "Bearer " <> token & encodeUtf8)]
      Nothing -> []


execGithubGqlQuery :: Maybe Text -> Text -> KeyMap Value -> [Repo] -> IO [Repo]
execGithubGqlQuery ghTokenMb query variables initialRepos = do
  putText "\n▶️ Query a batch of repos from GitHub …"

  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest $ T.unpack "https://api.github.com/graphql"

  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = getGhHeaders ghTokenMb
          , requestBody =
              RequestBodyLBS $
                encode $
                  object
                    [ "query" .= query
                    , "variables" .= variables
                    ]
          }

  response <- httpLbs request manager

  let gqlResult :: Either String GqlRepoRes =
        response.responseBody & eitherDecode

  case gqlResult of
    Left error -> do
      putErrText $ "HTTP Error: " <> show error
      pure []
    Right gqlResponse -> do
      case gqlResponse.errorsMb of
        Just errors -> putErrText $ "GraphQL Errors:\n" <> show errors
        Nothing -> pure ()

      let repos :: [Repo] = gqlResponse.repos

      putStrLn $
        "✅ Received "
          <> showInt (P.length repos) " repos "
          <> "from GitHub"

      repos
        <&> ( \repo ->
                (repo.owner & fromMaybe "")
                  <> ("/" :: Text)
                  <> (repo.name & fromMaybe "")
                  <> (" | stars: " :: Text)
                  <> show repo.stargazerCount
                  <> (" | commits: " :: Text)
                  <> ( repo.commitsCount
                        <&> show
                        & fromMaybe "ERROR: Should have a commits count"
                     )
            )
        & mapM_ putText

      when (P.not $ P.null repos) $ do
        saveReposInAirsequel OverwriteRepo repos

      case gqlResponse.nextCursorMb of
        Nothing -> pure $ initialRepos <> repos
        Just nextCursor -> do
          execGithubGqlQuery
            ghTokenMb
            query
            (variables & KeyMap.insert "after" (String nextCursor))
            (initialRepos <> repos)


loadAndSaveReposViaSearch
  :: Maybe Text
  -> Text
  -> Int
  -> Maybe Text
  -> IO [Repo]
loadAndSaveReposViaSearch ghTokenMb searchQuery numRepos afterMb = do
  let gqlQUery =
        [r|
          query SearchRepos(
            $searchQuery: String!
            $numRepos: Int!
            $after: String
          ){
            search(
              query: $searchQuery
              type: REPOSITORY
              first: $numRepos
              after: $after
            ) {
              edges {
                node {
                  ... on Repository {
                    owner { login }
                    name
                    databaseId
                    stargazerCount
                    description
                    homepageUrl
                    primaryLanguage { name }
                    # languages(first: 10) {
                    #   totalCount
                    #   nodes { name }
                    # }
                    issues (states: [OPEN]) {
                      totalCount
                    }
                    isArchived
                    createdAt
                    updatedAt
                    defaultBranchRef {
                      target {
                        ... on Commit {
                          history {
                            totalCount
                          }
                        }
                      }
                    }
                  }
                }
              }
              pageInfo {
                endCursor
              }
            }
          }
        |]

  execGithubGqlQuery
    ghTokenMb
    gqlQUery
    ( KeyMap.fromList
        [ "searchQuery" .= searchQuery
        , "numRepos" .= numRepos
        , "after" .= afterMb
        ]
    )
    []


-- | Function to handle the execution of commands
run :: CliCmd -> IO ()
run cliCmd = do
  ghTokenMb <- loadGitHubToken

  case cliCmd of
    Upload repoSlug -> do
      let
        fragments = repoSlug & T.splitOn "/"
        ownerMb = fragments & headMay
        nameMb = fragments & lastMay

      case ownerMb of
        Nothing -> putErrText "Error: Repo owner is missing"
        Just owner ->
          case nameMb of
            Nothing -> putErrText "Error: Repo name is missing"
            Just name -> do
              _ <-
                loadAndSaveRepo
                  ghTokenMb
                  OverwriteRepo
                  owner
                  name
              pure ()
    Search searchQuery -> do
      let searchQueryNorm = searchQuery & T.replace "\n" " " & T.strip

      repos <- loadAndSaveReposViaSearch ghTokenMb searchQueryNorm 20 Nothing

      putStrLn $
        "🏁 Total number of crawled repos: "
          <> showInt (P.length repos) ""

      pure ()


main :: IO ()
main = do
  let opts =
        info
          (commands <**> helper)
          ( fullDesc
              <> headerDoc (Just "⬆️ Repos Uploader")
              <> progDesc
                "Crawl repos from GitHub and upload their metadata to Airsequel"
          )

  execParser opts >>= run
