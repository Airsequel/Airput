{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Use maybe" #-}

module Main where

import Protolude (
  Either (Left, Right),
  IO,
  Int,
  Integer,
  Maybe (..),
  Text,
  encodeUtf8,
  fromMaybe,
  headMay,
  lastMay,
  many,
  mapM_,
  mempty,
  pure,
  putErrText,
  putText,
  show,
  when,
  ($),
  (&),
  (<$>),
  (<&>),
  (<*>),
  (<>),
  (>),
  (>>=),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (Value (String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import GHC.Base (String)
import Network.HTTP.Client (
  RequestBody (RequestBodyLBS),
  httpLbs,
  method,
  newManager,
  parseRequest,
  requestBody,
  requestHeaders,
  responseBody,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative (
  Parser,
  argument,
  command,
  execParser,
  fullDesc,
  headerDoc,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  progDesc,
  progDescDoc,
  showDefault,
  some,
  str,
  strOption,
  value,
  (<**>),
 )
import Options.Applicative.Help.Pretty (vsep)
import Text.RawString.QQ (r)

import Airsequel (saveReposInAirsequel)
import FileUploader (uploadFiles)
import Types (GqlRepoRes (..), Repo (..), SaveStrategy (..))
import Utils (loadGitHubToken)


data CliCmd
  = -- | Upload files
    FileUpload
      { domain :: [P.Char]
      , dbId :: [P.Char]
      , tableName :: [P.Char]
      , paths :: [P.FilePath]
      }
  | -- | Upload metadata for a single GitHub repo
    GithubUpload Text
  | -- | Search for GitHub repos and upload their metadata
    GithubSearch [Text]


commands :: Parser CliCmd
commands = do
  let
    fileUpload :: Parser CliCmd
    fileUpload =
      FileUpload
        <$> strOption
          ( long "domain"
              <> metavar "DOMAIN_NAME"
              <> help "Domain to upload files to"
              <> showDefault
              <> value "https://www.airsequel.com"
          )
        <*> strOption
          ( long "dbid"
              <> metavar "DATABASE_ID"
              <> help "Database ID to upload files to"
          )
        <*> strOption
          ( long "tablename"
              <> metavar "TABLE_NAME"
              <> help "Table name to upload files to"
          )
        <*> some (argument str (metavar "FILE/DIR..."))

    githubUpload :: Parser CliCmd
    githubUpload =
      GithubUpload <$> argument str (metavar "REPO_SLUG")

    githubSearch :: Parser CliCmd
    githubSearch =
      GithubSearch <$> many (argument str (metavar "SEARCH_QUERY"))

  hsubparser
    ( mempty
        <> command
          "upload"
          ( info
              fileUpload
              ( progDesc
                  "Upload files to a database via the REST API. \
                  \Expects 3 columns: `name`, `filetype`, and `content`."
              )
          )
        <> command
          "github-upload"
          ( info
              githubUpload
              (progDesc "Upload metadata for a single GitHub repo")
          )
        <> command
          "github-search"
          ( info
              githubSearch
              ( progDescDoc $
                  Just $
                    vsep
                      [ "Search for GitHub repos and upload their metadata."
                      , ""
                      , "If several search queries are provided, they will be"
                      , "  executed one after the other."
                      , ""
                      , "WARNING: If a search returns more than 1000 repos,"
                      , "  the results will be truncated."
                      , ""
                      , "Good search options are:"
                      , "- language:haskell"
                      , "- stars:>=10"
                      , "- stars:10..50"
                      , "- created:2023-10"
                      , "- archived:true"
                      ]
              )
          )
    )


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
  [ ("User-Agent", "airput")
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

      when (P.null initialRepos {- First call -}) $ do
        putText $
          "\n📲 Total number of repos: "
            <> show @Integer gqlResponse.repositoryCount

        when (gqlResponse.repositoryCount > 1000) $ do
          putText $
            "\n⚠️ WARNING\n"
              <> "⚠️ The search returns more than 1000 repos.\n"
              <> "⚠️ Not all repos will be crawled.\n"

      let repos :: [Repo] = gqlResponse.repos

      putText $
        "\n✅ Received "
          <> show @Int (P.length repos)
          <> " repos from GitHub"

      repos
        <&> ( \repo ->
                T.replicate 4 " "
                  <> (repo.owner & fromMaybe "")
                  <> ("/" :: Text)
                  <> (repo.name & fromMaybe "")
                  <> (" | stars: " :: Text)
                  <> show @Integer (repo.stargazerCount & fromMaybe 0)
                  <> (" | commits: " :: Text)
                  <> ( repo.commitsCount
                        <&> show @Integer
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
              repositoryCount
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
  case cliCmd of
    FileUpload{domain, dbId, tableName, paths} -> do
      uploadFiles (T.pack domain) (T.pack dbId) (T.pack tableName) paths
    --
    GithubUpload repoSlug -> do
      ghTokenMb <- loadGitHubToken
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
    --
    GithubSearch searchQueries -> do
      ghTokenMb <- loadGitHubToken
      let searchQueriesNorm = searchQueries <&> (T.replace "\n" " " >>> T.strip)

      allRepos <- P.forM searchQueriesNorm $ \searchQueryNorm -> do
        repos <- loadAndSaveReposViaSearch ghTokenMb searchQueryNorm 50 Nothing

        putText $
          "\n🏁 Crawled "
            <> show @Int (P.length repos)
            <> " repos with search query:\n"
            <> searchQueryNorm
            <> "\n"

        pure repos

      putText $
        "\n🏁🏁🏁 Crawled "
          <> show @Int (P.length $ P.concat allRepos)
          <> " repos in total 🏁🏁🏁\n"


main :: IO ()
main = do
  let opts =
        info
          (commands <**> helper)
          ( fullDesc
              <> headerDoc (Just "⬆️ Airput ⬆️")
              <> progDesc
                "CLI tool for populating Airsequel with data."
          )

  execParser opts >>= run
