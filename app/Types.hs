module Types where

import Protolude (
  Bool (False),
  Eq,
  Generic,
  Int,
  Integer,
  Maybe (..),
  Proxy (Proxy),
  Show,
  Text,
  mapM,
  pure,
  ($),
  (&),
  (<&>),
  (>>=),
 )

import Data.Aeson (FromJSON, Value, withObject, (.:), (.:?))
import Data.Aeson.Types (parseJSON)
import Data.Time (UTCTime)
import GitHub qualified as GH


emptyOwner :: GH.SimpleOwner
emptyOwner =
  GH.SimpleOwner
    { GH.simpleOwnerLogin = ""
    , GH.simpleOwnerAvatarUrl = GH.URL ""
    , GH.simpleOwnerId = GH.mkId (Proxy :: Proxy GH.Owner) 0
    , GH.simpleOwnerUrl = GH.URL ""
    , GH.simpleOwnerType = GH.OwnerUser
    }


emptyRepo :: GH.Repo
emptyRepo =
  GH.Repo
    { GH.repoOwner = emptyOwner
    , GH.repoArchived = False
    , GH.repoCloneUrl = Nothing
    , GH.repoCreatedAt = Nothing
    , GH.repoDefaultBranch = Nothing
    , GH.repoDescription = Nothing
    , GH.repoDisabled = False
    , GH.repoForksCount = 0
    , GH.repoHasDownloads = Nothing
    , GH.repoHasIssues = Nothing
    , GH.repoHasPages = Nothing
    , GH.repoHasProjects = Nothing
    , GH.repoHasWiki = Nothing
    , GH.repoHomepage = Nothing
    , GH.repoHooksUrl = GH.URL ""
    , GH.repoHtmlUrl = GH.URL ""
    , GH.repoId = GH.mkId (Proxy :: Proxy GH.Repo) 0
    , GH.repoLanguage = Nothing
    , GH.repoName = ""
    , GH.repoOpenIssuesCount = 0
    , GH.repoPermissions = Nothing
    , GH.repoPushedAt = Nothing
    , GH.repoSize = Nothing
    , GH.repoStargazersCount = 0
    , GH.repoSvnUrl = Nothing
    , GH.repoUpdatedAt = Nothing
    , GH.repoWatchersCount = 0
    , GH.repoPrivate = False
    , GH.repoFork = Nothing
    , GH.repoUrl = GH.URL ""
    , GH.repoGitUrl = Nothing
    , GH.repoSshUrl = Nothing
    }


{-| To make loading data from GitHub GraphQL API easier
| we also have this simpler (in comparison to GH.Repo) data type
-}
data RepoObject = RepoObject
  { owner :: Text
  , name :: Text
  , githubId :: Int
  , stargazerCount :: Int
  , description :: Maybe Text
  , homepageUrl :: Maybe Text
  , issuesCount :: Int
  , isArchived :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)


instance FromJSON RepoObject where
  parseJSON = withObject "RepoObject" $ \o -> do
    owner <- o .: "owner" >>= (.: "login")
    name <- o .: "name"
    githubId <- o .: "databaseId"
    stargazerCount <- o .: "stargazerCount"
    description <- o .: "description"
    homepageUrl <- o .: "homepageUrl"
    issuesCount <- o .: "issues" >>= (.: "totalCount")
    isArchived <- o .: "isArchived"
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"

    pure RepoObject{..}


repoObjectToRepo :: RepoObject -> GH.Repo
repoObjectToRepo repoObj =
  emptyRepo
    { GH.repoOwner =
        emptyOwner
          { GH.simpleOwnerLogin =
              GH.mkOwnerName repoObj.owner
          }
    , GH.repoName = GH.mkRepoName repoObj.name
    , GH.repoId = GH.mkId (Proxy :: Proxy GH.Repo) repoObj.githubId
    , GH.repoHomepage = repoObj.homepageUrl
    , GH.repoDescription = repoObj.description
    , GH.repoStargazersCount = repoObj.stargazerCount
    , GH.repoOpenIssuesCount = repoObj.issuesCount
    , GH.repoArchived = repoObj.isArchived
    , GH.repoCreatedAt = Just repoObj.createdAt
    , GH.repoUpdatedAt = Just repoObj.updatedAt
    }


data GqlResponse = GqlResponse
  { repos :: [GH.Repo]
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
    repos :: [RepoObject] <- edges & mapM (.: "node")

    pageInfo <- search .: "pageInfo"
    nextCursorMb <- pageInfo .:? "endCursor"

    pure
      GqlResponse
        { repos = repos <&> repoObjectToRepo
        , errorsMb
        , nextCursorMb
        }


data SaveStrategy = OverwriteRepo | AddRepo
  deriving (Show, Eq)


data ExtendedRepo = ExtendedRepo
  { core :: GH.Repo
  , commitsCount :: Maybe Integer
  }
  deriving (Show, Eq)
