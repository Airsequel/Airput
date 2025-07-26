module Types where

import Protolude (
  Bool (..),
  Eq,
  Generic,
  Integer,
  Maybe (..),
  Show,
  Text,
  mapM,
  pure,
  ($),
  (&),
  (>>=),
 )

import Data.Aeson (FromJSON, Object, Value, withObject, (.:), (.:?))
import Data.Aeson.Types (parseJSON)
import Data.Time (UTCTime)


data Repo = Repo
  { githubId :: Integer
  , rowid :: Maybe Integer -- Airsequel rowid
  , owner :: Maybe Text
  , name :: Maybe Text
  , stargazerCount :: Maybe Integer
  , description :: Maybe Text
  , homepageUrl :: Maybe Text
  , primaryLanguage :: Maybe Text
  , openIssuesCount :: Maybe Integer
  , openPrsCount :: Maybe Integer
  , isPrivate :: Maybe Bool
  , isArchived :: Maybe Bool
  , createdAt :: Maybe UTCTime
  , updatedAt :: Maybe UTCTime
  , pushedAt :: Maybe UTCTime
  , commitsCount :: Maybe Integer
  }
  deriving (Show, Eq, Generic)


emptyRepo :: Repo
emptyRepo =
  Repo
    { githubId = 0
    , rowid = Nothing
    , owner = Nothing
    , name = Nothing
    , stargazerCount = Nothing
    , description = Nothing
    , homepageUrl = Nothing
    , primaryLanguage = Nothing
    , openIssuesCount = Nothing
    , openPrsCount = Nothing
    , isPrivate = Nothing
    , isArchived = Nothing
    , createdAt = Nothing
    , updatedAt = Nothing
    , pushedAt = Nothing
    , commitsCount = Nothing
    }


instance FromJSON Repo where
  parseJSON = withObject "RepoObject" $ \o -> do
    githubId <- o .: "databaseId"
    rowid <- o .:? "rowid"

    ownerMb <- o .:? "owner"
    owner <- case ownerMb of
      Nothing -> pure Nothing
      Just ownerObj -> ownerObj .: "login"

    nameMb <- o .:? "name"
    name <- case nameMb of
      Nothing -> pure Nothing
      Just name -> pure $ Just name

    stargazerCount <- o .:? "stargazerCount"
    description <- o .:? "description"
    homepageUrl <- o .:? "homepageUrl"

    primaryLanguageMb <- o .:? "primaryLanguage"
    primaryLanguage <- case primaryLanguageMb of
      Nothing -> pure Nothing
      Just langObj -> langObj .: "name"

    openIssuesCountMb <- o .:? "issues"
    openIssuesCount <- case openIssuesCountMb of
      Nothing -> pure Nothing
      Just issuesObj -> issuesObj .: "totalCount"

    openPrsCountMb <- o .:? "pullRequests"
    openPrsCount <- case openPrsCountMb of
      Nothing -> pure Nothing
      Just prsObj -> prsObj .: "totalCount"

    isPrivate <- o .:? "isPrivate"
    isArchived <- o .:? "isArchived"
    createdAt <- o .:? "createdAt"
    updatedAt <- o .:? "updatedAt"
    pushedAt <- o .:? "pushedAt"

    defaultBranchRef <- o .:? "defaultBranchRef"
    commitsCount <- case defaultBranchRef of
      Nothing -> pure Nothing
      Just branchRef ->
        branchRef
          .: "target"
          >>= (.: "history")
          >>= (.: "totalCount")

    pure Repo{..}


-- | Generic GraphQL response
data GqlRes = GqlRes
  { gqlData :: Maybe Value
  , gqlErrors :: Maybe [Object]
  }
  deriving (Show, Eq, Generic)


instance FromJSON GqlRes where
  parseJSON = withObject "GqlRes" $ \o -> do
    gqlData <- o .:? "data"
    gqlErrors <- o .:? "errors"
    pure GqlRes{..}


data GqlRepoRes = GqlRepoRes
  { repositoryCount :: Integer
  , repos :: [Repo]
  , errorsMb :: Maybe Value
  , nextCursorMb :: Maybe Text
  }
  deriving (Show, Eq, Generic)


instance FromJSON GqlRepoRes where
  parseJSON = withObject "GqlRepoRes" $ \o -> do
    data_ <- o .: "data"
    errorsMb <- o .:? "errors"
    searchMb <- data_ .:? "search"

    case searchMb of
      Nothing -> do
        repository <- data_ .: "repository"
        repo <- parseJSON repository
        pure
          GqlRepoRes
            { repositoryCount = 1
            , repos = [repo]
            , errorsMb
            , nextCursorMb = Nothing
            }
      Just search -> do
        repositoryCount <- search .: "repositoryCount"
        edges <- search .: "edges"
        repos :: [Repo] <- edges & mapM (.: "node")

        pageInfo <- search .: "pageInfo"
        nextCursorMb <- pageInfo .:? "endCursor"

        pure
          GqlRepoRes
            { repositoryCount
            , repos
            , errorsMb
            , nextCursorMb
            }


data SaveStrategy = OverwriteRepo {tableName :: Text} | AddRepo {tableName :: Text}
  deriving (Show, Eq)
