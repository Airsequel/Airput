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
  Integer,
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
  putText,
  readMaybe,
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

import Control.Arrow ((>>>))
import Data.Aeson (Value (String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (lookup)
import Data.Text qualified as T
import GHC.Base (String)
import GitHub qualified as GH
import GitHub.Endpoints.Activity.Starring as GH (Auth (OAuth), Repo, untagName)
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
import Options.Applicative (
  Parser,
  argument,
  command,
  execParser,
  fullDesc,
  header,
  helper,
  hsubparser,
  info,
  metavar,
  progDesc,
  str,
  (<**>),
 )
import Text.RawString.QQ (r)

import Airsequel (saveRepoInAirsequel)
import Types (ExtendedRepo (..), GqlResponse (..), SaveStrategy (..))
import Utils (loadGitHubToken, mapMSequentially)


-- TODO: Add CLI flag to choose between OverwriteRepo and AddRepo
data CliCmd
  = -- | Upload a single repo
    Upload Text
  | -- | Search for repos and upload them
    Search


commands :: Parser CliCmd
commands = do
  let
    upload :: Parser CliCmd
    upload = Upload <$> argument str (metavar "REPO_SLUG")

    search :: Parser CliCmd
    search = pure Search

  hsubparser
    ( mempty
        <> command
          "upload"
          (info upload (progDesc "Upload a single repo"))
        <> command
          "search"
          ( info
              search
              ( progDesc
                  "Search for and upload several repos.\n\
                  \WARNING: If the search query returns too many repos, \
                  \the result will be truncated."
              )
          )
    )


formatRepo :: ExtendedRepo -> Text
formatRepo extendedRepo =
  let
    repo = core extendedRepo
  in
    "\n\n"
      <> ("repo_url: " <> show (GH.repoHtmlUrl repo) <> "\n")
      <> ( "description: "
            <> (repo.repoDescription & fromMaybe "")
            <> "\n"
         )
      <> ("homepage: " <> (repo.repoHomepage & fromMaybe "") <> "\n")
      <> ( "language: "
            <> (repo.repoLanguage <&> GH.getLanguage & fromMaybe "")
            <> "\n"
         )
      <> ("stargazers_count: " <> show (GH.repoStargazersCount repo) <> "\n")
      <> ( "commits_count: "
            <> show (extendedRepo & commitsCount & fromMaybe 0)
            <> "\n"
         )
      <> ("open_issues_count: " <> show (GH.repoOpenIssuesCount repo) <> "\n")
      <> ( "is_archived: " <> (repo.repoArchived & show & T.toLower) <> "\n"
         )
      <> ( "created_at: "
            <> (repo.repoCreatedAt <&> show & fromMaybe "")
            <> "\n"
         )
      <> ( "updated_at: "
            <> (repo.repoUpdatedAt <&> show & fromMaybe "")
            <> "\n"
         )


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


{-| Workaround to get the number of commits for a repo
| https://stackoverflow.com/a/70610670
-}
getNumberOfCommits :: Maybe Text -> Repo -> IO (Maybe Integer)
getNumberOfCommits ghTokenMb repo = do
  let repoSlug =
        (repo.repoOwner.simpleOwnerLogin & untagName)
          <> "/"
          <> (repo.repoName & untagName)

  putText $ "⏳ Get number of commits for repo " <> repoSlug

  let apiEndpoint =
        "https://api.github.com/repos/"
          <> repoSlug
          <> "/commits?per_page=1"

  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack apiEndpoint
  let request =
        initialRequest
          { method = "HEAD"
          , requestHeaders = getGhHeaders ghTokenMb
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


{-| Loads a single repo from GitHub, adds number of commits,
| and saves it to Airsequel
-}
loadAndSaveRepo :: Maybe Text -> SaveStrategy -> Text -> Text -> IO ()
loadAndSaveRepo ghTokenMb saveStrategy owner name = do
  let
    uploadFunc = case ghTokenMb of
      Nothing -> GH.github'
      Just ghToken -> GH.github (OAuth (encodeUtf8 ghToken))

  repoResult <-
    uploadFunc
      GH.repositoryR
      (fromString $ T.unpack owner)
      (fromString $ T.unpack name)

  case repoResult of
    Left error -> putErrText $ "Error: " <> show error
    Right repo -> do
      commitsCount <- getNumberOfCommits ghTokenMb repo
      let extendedRepo =
            ExtendedRepo
              { core = repo
              , commitsCount = commitsCount
              }
      putText $ formatRepo extendedRepo
      saveRepoInAirsequel saveStrategy extendedRepo


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


execGithubGqlQuery
  :: Maybe Text
  -> Text
  -> KeyMap Value
  -> [ExtendedRepo]
  -> IO [ExtendedRepo]
execGithubGqlQuery ghTokenMb query variables initialRepos = do
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

  let gqlResult :: Either String GqlResponse =
        response.responseBody & eitherDecode

  case gqlResult of
    Left error -> do
      putErrText $ "HTTP Error: " <> show error
      pure []
    Right gqlResponse -> do
      case gqlResponse.errorsMb of
        Just errors -> putErrText $ "GraphQL Errors:\n" <> show errors
        Nothing -> pure ()

      let
        repos :: [GH.Repo] = gqlResponse.repos
        delayBetweenRequests = 500 -- ms
      commitsCounts <-
        mapMSequentially
          delayBetweenRequests
          (getNumberOfCommits ghTokenMb)
          repos

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

      when (P.not $ P.null extendedRepos) $ do
        putText $
          "⏳ Save "
            <> show (P.length repos)
            <> " repos to Airsequel …"
        extendedRepos
          & mapM_ (saveRepoInAirsequel OverwriteRepo)

      case gqlResponse.nextCursorMb of
        Nothing -> pure $ initialRepos <> extendedRepos
        Just nextCursor -> do
          execGithubGqlQuery
            ghTokenMb
            query
            (variables & KeyMap.insert "after" (String nextCursor))
            (initialRepos <> extendedRepos)


loadAndSaveReposViaSearch
  :: Maybe Text
  -> Text
  -> Int
  -> Maybe Text
  -> IO [ExtendedRepo]
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
                    issues (states: [OPEN]) {
                      totalCount
                    }
                    isArchived
                    createdAt
                    updatedAt
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
            Just name ->
              loadAndSaveRepo
                ghTokenMb
                OverwriteRepo
                owner
                name
    Search -> do
      -- Good filter options:
      --   language:haskell
      --   stars:>=10
      --   stars:10..50
      --   sort:updated-desc
      --   sort:stars-asc
      --   archived:true
      let searchQuery =
            [r|
              language:haskell
              stars:56..76
              sort:stars-desc
            |]
              & T.replace "\n" " "
              & T.strip

      repos <- loadAndSaveReposViaSearch ghTokenMb searchQuery 20 Nothing

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

      pure ()


main :: IO ()
main = do
  let opts =
        info
          (commands <**> helper)
          ( fullDesc
              <> progDesc "Upload repo metadata to Airsequel"
              <> header "repos-uploader"
          )

  execParser opts >>= run
