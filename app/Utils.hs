module Utils (
  loadAirsWriteToken,
  loadDbEndpoint,
  loadDbId,
  loadGitHubToken,
  mapMSequentially,
)
where

import Protolude (
  IO,
  Int,
  Maybe (..),
  Text,
  liftIO,
  mapM,
  pure,
  putErrText,
  ($),
  (*),
  (<*),
  (<>),
 )

import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Exit (die)


lookupEnvOrDie :: Text -> IO Text
lookupEnvOrDie envVarName = do
  envVarMb <- lookupEnv (T.unpack envVarName)
  case envVarMb of
    Just envVar -> pure $ T.pack envVar
    Nothing -> do
      die $
        T.unpack $
          "ERROR: "
            <> envVarName
            <> " environment variable must be set"


-- | The ID of the Airsequel database loaded from the environment
loadDbId :: IO Text
loadDbId =
  lookupEnvOrDie "AIRSEQUEL_DB_ID"


loadDbEndpoint :: IO Text
loadDbEndpoint = do
  dbId <- loadDbId
  pure $ "https://www.airsequel.com/dbs/" <> dbId <> "/graphql"


loadAirsWriteToken :: IO Text
loadAirsWriteToken =
  lookupEnvOrDie "AIRSEQUEL_API_TOKEN"


loadGitHubToken :: IO (Maybe Text)
loadGitHubToken = do
  ghTokenMb <- lookupEnv "GITHUB_TOKEN"
  case ghTokenMb of
    Nothing -> do
      putErrText
        "WARNING: Without a GITHUB_TOKEN environment variable \
        \all requests to GitHub will be unauthenticated."
      pure Nothing
    Just token ->
      pure $ Just $ T.pack token


mapMSequentially :: Int -> (a -> IO b) -> [a] -> IO [b]
mapMSequentially delayInMs f xs = do
  let delayM = liftIO $ threadDelay (delayInMs * 1000)
  mapM (\x -> f x <* delayM) xs
