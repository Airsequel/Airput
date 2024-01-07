module Types where

import Protolude (
  Bool (False),
  Eq,
  Generic,
  Int,
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
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)


data Repo = Repo
  { rowid :: Maybe Int -- Airsequel rowid
  , owner :: Text
  , name :: Text
  , githubId :: Int
  , stargazerCount :: Int
  , description :: Maybe Text
  , homepageUrl :: Maybe Text
  , primaryLanguage :: Maybe Text
  , openIssuesCount :: Int
  , isArchived :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , commitsCount :: Maybe Integer
  }
  deriving (Show, Eq, Generic)


emptyRepo :: Repo
emptyRepo =
  Repo
    { rowid = Nothing
    , owner = ""
    , name = ""
    , githubId = 0
    , stargazerCount = 0
    , description = Nothing
    , homepageUrl = Nothing
    , primaryLanguage = Nothing
    , openIssuesCount = 0
    , isArchived = False
    , createdAt = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
    , updatedAt = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
    , commitsCount = Nothing
    }


instance FromJSON Repo where
  parseJSON = withObject "RepoObject" $ \o -> do
    owner <- o .: "owner" >>= (.: "login")
    name <- o .: "name"
    githubId <- o .: "databaseId"
    stargazerCount <- o .: "stargazerCount"
    description <- o .: "description"
    homepageUrl <- o .: "homepageUrl"
    primaryLanguage <- o .: "primaryLanguage" >>= (.: "name")
    openIssuesCount <- o .: "issues" >>= (.: "totalCount")
    isArchived <- o .: "isArchived"
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"
    commitsCount <-
      o .: "defaultBranchRef"
        >>= (.: "target")
        >>= (.: "history")
        >>= (.: "totalCount")

    pure Repo{rowid = Nothing, ..}


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
  { repos :: [Repo]
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
            { repos = [repo]
            , errorsMb
            , nextCursorMb = Nothing
            }
      Just search -> do
        edges <- search .: "edges"
        repos :: [Repo] <- edges & mapM (.: "node")

        pageInfo <- search .: "pageInfo"
        nextCursorMb <- pageInfo .:? "endCursor"

        pure
          GqlRepoRes
            { repos = repos
            , errorsMb
            , nextCursorMb
            }


data SaveStrategy = OverwriteRepo | AddRepo
  deriving (Show, Eq)
