{-# LANGUAGE MultiWayIf #-}

{-| Load the weekly star history of GitHub repos via
<https://docs.github.com/en/rest/activity/starring#get-repository-star-history>
-}
module StarHistory (
  loadStarHistory,
  loadAndSaveStarHistories,
)
where

import Protolude (
  Bool (..),
  Double,
  Either (..),
  IO,
  Int,
  Integer,
  Maybe (..),
  Text,
  ceiling,
  fromIntegral,
  fromMaybe,
  max,
  otherwise,
  pure,
  putErrText,
  putText,
  readMaybe,
  realToFrac,
  show,
  when,
  ($),
  (&),
  (&&),
  (*),
  (+),
  (-),
  (/),
  (<),
  (<&>),
  (<=),
  (<>),
  (==),
  (>),
  (>=),
  (^),
  (||),
 )
import Protolude qualified as P

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Client (
  Manager,
  Request,
  Response,
  httpLbs,
  newManager,
  parseRequest,
  requestHeaders,
  responseBody,
  responseHeaders,
  responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName, statusCode)

import Airsequel (loadStoredStarHistory, replaceStarHistory)
import Types (Repo (..), StarWeek (..))
import Utils (getGhHeaders)


-- | Maximum values allowed by the GitHub API
perPage, maxPage :: Int
perPage = 30
maxPage = 100


{-| Load the star history of a repo (newest week first) starting at `startPage`.
Stops early once `isEnough` is true for a loaded page.
Returns the loaded weeks and the next page to load
(`Nothing` if the history is complete),
or `Nothing` if it could not be loaded.
-}
loadStarHistory ::
  Manager ->
  Maybe Text ->
  Text ->
  Text ->
  ([StarWeek] -> Bool) ->
  Int ->
  IO (Maybe ([StarWeek], Maybe Int))
loadStarHistory manager ghTokenMb owner name isEnough startPage =
  go startPage []
  where
    go :: Int -> [StarWeek] -> IO (Maybe ([StarWeek], Maybe Int))
    go page weeksAcc = do
      initialRequest <-
        parseRequest $
          T.unpack $
            "https://api.github.com/repos/"
              <> owner
              <> "/"
              <> name
              <> "/stargazers/history?per_page="
              <> show perPage
              <> "&page="
              <> show page

      result <-
        fetchWithRetries
          manager
          initialRequest{requestHeaders = getGhHeaders ghTokenMb}

      case result of
        Left reason -> do
          putErrText $
            "❌ Loading star history of "
              <> owner
              <> "/"
              <> name
              <> " failed: "
              <> reason
          pure Nothing
        Right weeks
          | P.length weeks < perPage || page >= maxPage ->
              pure $ Just (weeksAcc <> weeks, Nothing)
          | isEnough weeks ->
              pure $ Just (weeksAcc <> weeks, Just $ page + 1)
          | otherwise ->
              go (page + 1) (weeksAcc <> weeks)


{-| Execute a GitHub REST request.

- Sleeps after every response to spread the remaining requests evenly
  until the rate limit resets (or until `Retry-After` has passed).
- Retries without counting an attempt when the rate limit was hit.
- Retries on network exceptions, HTTP 5xx, 422 (spam protection),
  and non-JSON response bodies.
-}
fetchWithRetries :: Manager -> Request -> IO (Either Text [StarWeek])
fetchWithRetries manager request = go 1
  where
    maxAttempts :: Int
    maxAttempts = 5

    go :: Int -> IO (Either Text [StarWeek])
    go attempt = do
      resultE :: Either SomeException (Response ByteString) <-
        try (httpLbs request manager)
      case resultE of
        Left ex ->
          retryOr attempt $ "network exception: " <> T.pack (show ex)
        Right response -> do
          let
            code = response.responseStatus.statusCode
            isRateLimited =
              (code == 403 || code == 429)
                && ( P.isJust (readHeader "Retry-After" response)
                       || readHeader "X-RateLimit-Remaining" response == Just 0
                   )

          waitForRateLimit response

          if
            | isRateLimited -> go attempt
            | code >= 500 || code == 422 ->
                retryOr attempt $ "HTTP " <> show code
            | code P./= 200 ->
                pure $ Left $ "HTTP " <> show code
            | otherwise ->
                case eitherDecode response.responseBody of
                  Left decodeErr ->
                    retryOr attempt $ "JSON decode error: " <> T.pack decodeErr
                  Right weeks -> pure $ Right weeks

    retryOr :: Int -> Text -> IO (Either Text [StarWeek])
    retryOr attempt reason
      | attempt >= maxAttempts = pure $ Left reason
      | otherwise = do
          let delaySeconds = (2 :: Int) ^ attempt
          putErrText $
            "⚠️  "
              <> reason
              <> " (attempt "
              <> show attempt
              <> "/"
              <> show maxAttempts
              <> "), retrying in "
              <> show delaySeconds
              <> "s…"
          threadDelay $ delaySeconds * 1_000_000
          go $ attempt + 1


-- | Read an integer header like `X-RateLimit-Remaining`
readHeader :: HeaderName -> Response a -> Maybe Integer
readHeader headerName response =
  response.responseHeaders
    & L.lookup headerName
      P.>>= readMaybe


{-| Wait until the `Retry-After` time has passed or else
long enough to spread the remaining requests evenly
until the rate limit resets.
-}
waitForRateLimit :: Response a -> IO ()
waitForRateLimit response = do
  now <- getPOSIXTime
  let
    delaySeconds :: Double
    delaySeconds =
      case readHeader "Retry-After" response of
        Just retryAfter -> fromIntegral retryAfter
        Nothing ->
          case ( readHeader "X-RateLimit-Remaining" response
               , readHeader "X-RateLimit-Reset" response
               ) of
            (Just remaining, Just resetAt) ->
              max 0 (fromIntegral resetAt - realToFrac now)
                / fromIntegral (max 1 remaining)
            _ -> 0

  when (delaySeconds > 10) $
    putText $
      "⏳ Rate limit reached, waiting "
        <> show @Int (ceiling delaySeconds)
        <> "s …"

  threadDelay $ ceiling $ delaySeconds * 1_000_000


-- | Load the star histories of repos and save them in Airsequel
loadAndSaveStarHistories :: Maybe Text -> Text -> [Repo] -> IO ()
loadAndSaveStarHistories ghTokenMb tableName repos = do
  manager <- newManager tlsManagerSettings

  P.forM_ repos $ \repo ->
    case (repo.owner, repo.name) of
      (Just owner, Just name)
        | (repo.stargazerCount & fromMaybe 0) > 0 ->
            loadAndSaveStarHistory manager ghTokenMb tableName repo owner name
      _ -> pure ()


{-| Update the stored star history of a repo.

Only the weeks since the newest stored week are loaded,
as the history before that is already stored.
However, unstarring removes a star from the week it was added,
so the stored history becomes outdated.
This is detected by comparing the sum of all weeks with the star count,
in which case the complete history is loaded.
-}
loadAndSaveStarHistory ::
  Manager -> Maybe Text -> Text -> Repo -> Text -> Text -> IO ()
loadAndSaveStarHistory manager ghTokenMb tableName repo owner name = do
  storedMb <- loadStoredStarHistory manager tableName repo.githubId

  let
    loadFrom isEnough page =
      loadStarHistory manager ghTokenMb owner name isEnough page

    sumStars :: [StarWeek] -> Integer
    sumStars weeks = weeks <&> (.total) & P.sum

    logResult :: Text -> [StarWeek] -> IO ()
    logResult mode weeks =
      putText $
        "⭐ "
          <> owner
          <> "/"
          <> name
          <> " | "
          <> mode
          <> " | loaded weeks: "
          <> show @Int (P.length weeks)

    loadAndReplaceAll :: Text -> IO ()
    loadAndReplaceAll mode = do
      loadedMb <- loadFrom (P.const False) 1
      P.forM_ loadedMb $ \(weeks, _) -> do
        logResult mode weeks
        replaceStarHistory manager tableName repo.githubId Nothing weeks

  case storedMb of
    Nothing -> pure () -- Error was already reported
    Just stored ->
      case newestStoredWeek stored of
        Nothing -> loadAndReplaceAll "full"
        Just (cutoffText, cutoff) -> do
          loadedMb <- loadFrom (P.any (\week -> week.weekStart <= cutoff)) 1

          P.forM_ loadedMb $ \(weeks, nextPageMb) -> do
            let
              newWeeks = weeks & P.filter (\week -> week.weekStart >= cutoff)
              storedStarsBefore =
                stored
                  & P.filter (\(weekText, _) -> weekText < cutoffText)
                    <&> P.snd
                  & P.sum

            if storedStarsBefore + sumStars newWeeks
              == (repo.stargazerCount & fromMaybe 0)
              then do
                logResult "incremental" weeks
                replaceStarHistory
                  manager
                  tableName
                  repo.githubId
                  (Just cutoffText)
                  newWeeks
              else do
                -- Reuse the already loaded pages
                restMb <- case nextPageMb of
                  Nothing -> pure $ Just []
                  Just nextPage ->
                    loadFrom (P.const False) nextPage <&> P.fmap P.fst
                P.forM_ restMb $ \rest -> do
                  logResult "full (stored history outdated)" (weeks <> rest)
                  replaceStarHistory
                    manager
                    tableName
                    repo.githubId
                    Nothing
                    (weeks <> rest)


{-| Newest stored week as stored text and as parsed time.
`Nothing` if no week is stored or it can't be parsed.
-}
newestStoredWeek :: [(Text, Integer)] -> Maybe (Text, UTCTime)
newestStoredWeek stored = do
  newestText <- stored <&> P.fst & P.maximumMay
  newest <- iso8601ParseM $ T.unpack newestText
  pure (newestText, newest)
