module Utils (emptyOwner, emptyRepo, mapMSequentially)
where

import Protolude (
  Bool (False),
  IO,
  Int,
  Maybe (Nothing),
  Proxy (Proxy),
  liftIO,
  mapM,
  ($),
  (*),
  (<*),
 )

import Control.Concurrent (threadDelay)
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
  let delayM = liftIO $ threadDelay (delayInMs * 1000) -- Delay in milliseconds
  mapM (\x -> f x <* delayM) xs
