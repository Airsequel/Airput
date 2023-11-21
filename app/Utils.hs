module Utils (
  emptyOwner,
  emptyRepo,
  mapMSequentially,
  RepoObject (..),
  repoObjectToRepo,
)
where

import Protolude (
  Bool (False),
  Eq,
  Generic,
  IO,
  Int,
  Maybe (Just, Nothing),
  Proxy (Proxy),
  Show,
  Text,
  fromMaybe,
  liftIO,
  mapM,
  pure,
  ($),
  (&),
  (*),
  (<*),
  (>>=),
 )

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, withObject, (.:))
import Data.Aeson.Types (parseJSON)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GitHub.Data qualified as GH

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

mapMSequentially :: Int -> (a -> IO b) -> [a] -> IO [b]
mapMSequentially delayInMs f xs = do
  let delayM = liftIO $ threadDelay (delayInMs * 1000)
  mapM (\x -> f x <* delayM) xs

{- | To make loading data from GitHub GraphQL API easier
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
    , GH.repoCreatedAt = Just repoObj.createdAt
    , GH.repoUpdatedAt = Just repoObj.updatedAt
    }
