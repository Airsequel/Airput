module Utils (
  loadAsWriteToken,
  loadDbEndpoint,
  loadDbId,
  loadGitHubToken,
  mapMSequentially,
  var,
)
where

import Protolude (
  IO,
  Int,
  Maybe,
  Text,
  fromMaybe,
  liftIO,
  mapM,
  pure,
  ($),
  (*),
  (<&>),
  (<*),
  (<>),
 )

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import System.Environment (lookupEnv)


-- | The ID of the Airsequel database loaded from the environment
loadDbId :: IO Text
loadDbId =
  lookupEnv "AIRSEQUEL_DB_ID" <&> (fromMaybe "" >>> T.pack)


loadDbEndpoint :: IO Text
loadDbEndpoint = do
  dbId <- loadDbId
  pure $ "https://www.airsequel.com/dbs/" <> dbId <> "/graphql"


loadAsWriteToken :: IO Text
loadAsWriteToken =
  lookupEnv "AIRSEQUEL_API_TOKEN" <&> (fromMaybe "" >>> T.pack)


loadGitHubToken :: IO (Maybe Text)
loadGitHubToken =
  lookupEnv "GITHUB_TOKEN" <&> (<&> T.pack)


-- | Replaces a variable in a string with a value
var :: Text -> Text -> Text -> Text
var idName =
  T.replace ("<<" <> idName <> ">>")


mapMSequentially :: Int -> (a -> IO b) -> [a] -> IO [b]
mapMSequentially delayInMs f xs = do
  let delayM = liftIO $ threadDelay (delayInMs * 1000)
  mapM (\x -> f x <* delayM) xs
