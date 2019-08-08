{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | TMDB API calls.
module TMDBDump.Internal.Api
  ( Id
  , ApiKey
  -- * Exception
  , ApiStatus (..)
  , APIException (..)
  , APIExceptionContent(..)
  -- * RateLimit settings
  , RateLimit(rlRemaining, rlReset)
  , RateLimitSettings(minRetryDelay, maxRetries)
  , mkRateLimitSettings
  -- * API Requests
  , fetchMoviesC
  , fetchMovieChangesC
  , fetchYdayMovieIdsC
  , fetchMovieGenres
  ) where

import           Conduit                     hiding (throwM)
import           Control.Concurrent          (forkFinally, myThreadId,
                                              threadDelay)
import           Control.Concurrent.QSem
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Monad.Extra
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LB
import qualified Data.Conduit.List           as CL
import           Data.Conduit.Zlib
import           Data.Has
import           Data.IORef
import           Data.List                   (stripPrefix)
import           Data.Maybe                  (catMaybes, fromJust, fromMaybe)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day, addDays)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX       (POSIXTime, getCurrentTime,
                                              getPOSIXTime)
import           Data.Time.Format
import           Data.Tuple.Extra            (both)
import           GHC.Generics
import           GHC.IO.Exception            (IOErrorType (..),
                                              IOException (..))
import qualified Network.HTTP.Client         as HC
import qualified Network.HTTP.Client.Conduit as HCC
import           Network.HTTP.Types
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Read                   (readMaybe)
import           TMDBDump.Internal.Types

-- | Type representing identifiers used by TMDB API.
type Id = Int
-- | Type of API Key.
type ApiKey = T.Text

-- | TMDB API URL.
tmdbApiURL :: String
tmdbApiURL = "https://api.themoviedb.org/3/"

-- | Daily exports URL.
tmdbExportURL :: String
tmdbExportURL = "https://files.tmdb.org/"

mkApiRequest :: MonadThrow m => ApiKey -> String -> QueryText -> m HC.Request
mkApiRequest api_key path queries = do
  req <- HC.parseRequest (tmdbApiURL ++ path)
  return $
    req
      { HC.queryString =
          renderQuery True $
          queryTextToQuery (queries ++ [("api_key", Just api_key)])
      , HC.requestHeaders = [("Accept", "application/json")]
      }
{-# INLINE mkApiRequest #-}

-- | API status code for Not Found: https://www.themoviedb.org/documentation/api/status-codes
notFoundStatusCode :: Int
notFoundStatusCode = 34

-- | API Response Status.
data ApiStatus = ApiStatus
  { apiStatusCode    :: Int    -- ^ status code.
  , apiStatusMessage :: T.Text -- ^ status message.
  }
  deriving (Show, Generic)

instance FromJSON ApiStatus where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "api"}

-- | Exception content for 'APIException'.
data APIExceptionContent
  = APIRetryOverLimit
  | APIError (Maybe ApiStatus)
  deriving (Show)

-- | Exception raised when TMDB api requests fail.
data APIException = APIException HCC.Request APIExceptionContent
  deriving Show
instance Exception APIException

fromAPIError :: HC.Request -> HC.Response LB.ByteString -> APIException
fromAPIError req resp = APIException req (APIError (decode' $ HC.responseBody resp :: Maybe ApiStatus))

-- | Type used to control rate-limiting params for TMDB API Calls.
data RateLimit = RateLimit
  { rlRemaining :: Int       -- ^ Remaining calls to TMDB server without getting 429.
  , rlReset     :: POSIXTime -- ^ Unix Epoch at which the limits get reset.
  }
  deriving (Show, Eq)

-- | Using this instead of threading the state prevents misuse of 'rateLimitedhttpLbs' where different calls can have distinct 'RateLimit' values and it will not work as expected without a shared instance.
globalRateLimit :: IORef (Maybe RateLimit)
globalRateLimit = unsafePerformIO $ newIORef Nothing
{-# NOINLINE globalRateLimit #-}

-- | Get the current global RateLimit.
getGlobalRateLimit :: IO (Maybe RateLimit)
getGlobalRateLimit = readIORef globalRateLimit
{-# INLINE getGlobalRateLimit #-}

-- | Settings to control the behaviour of rate-limited requests.
data RateLimitSettings = RLSettings
  { minRetryDelay :: NominalDiffTime         -- ^ Minimum delay between retry requests.
  , maxRetries    :: Int                     -- ^ Maximum retries after which rateLimitedhttpLbs fails with RetryOverLimit Exception.
  }
  deriving (Show)

-- | mkRateLimitSettings @min_retry_delay@ @max_retries@ creates a new RateLimitSettings.
mkRateLimitSettings :: NominalDiffTime -> Int -> RateLimitSettings
mkRateLimitSettings = RLSettings

-- | generalized 'rateLimitedhttpsLbs'.
rateLimitedhttpLbsM ::
     ( MonadIO m
     , MonadReader env m
     , Has HC.Manager env
     , Has RateLimitSettings env
     )
  => HC.Request
  -> m (HC.Response LB.ByteString)
rateLimitedhttpLbsM req = do
  env <- ask
  let rlsettings = getter env
  let manager = getter env
  liftIO $ rateLimitedhttpLbs rlsettings manager req

-- | same as 'HC.httpLbs' but with rate-limits as enforced by the API server.
rateLimitedhttpLbs ::
     RateLimitSettings
  -> HC.Manager
  -> HC.Request
  -> IO (HC.Response LB.ByteString)
rateLimitedhttpLbs settings manager req = do
  rl_opt <- getGlobalRateLimit
  liftIO $
    case rl_opt of
      Nothing -> fetchResponse (maxRetries settings)
      Just rl -> do
        now <- getPOSIXTime
        mapM_ threadDelay (requestDelay rl now)
        fetchResponse (maxRetries settings)
  where
    fetchResponse !count = do
      resp <-
        HC.httpLbs req manager `catches`
        [ Handler $ \(_ :: HC.HttpException) ->
            retryRequest (minRetryDelay settings) count
        , Handler $ \(e :: IOException) ->
            if (ioe_type e) == ResourceVanished
              then retryRequest (minRetryDelay settings) count
              else throwM e
        ]
      let status = HC.responseStatus resp
      if status == toEnum 429
        then do
          now <- getPOSIXTime
          let retry_in =
                maybe (minRetryDelay settings) (max (minRetryDelay settings)) $
                getRetryAfter resp
          atomicModifyIORef'
            globalRateLimit
            (\x -> (updateRateLimit x (Just $ RateLimit 0 (now + retry_in)), ()))
          retryRequest retry_in count
        else do
          atomicModifyIORef'
            globalRateLimit
            (\x -> (updateRateLimit x $ getRateLimit resp, ()))
          return resp
    -- Retry request.
    retryRequest delay remaining = do
      (threadDelay . round . (* 1E6)) delay
      if remaining > 0
        then fetchResponse (remaining - 1)
        else throwM $ APIException req APIRetryOverLimit
    -- This is number of microseconds.
    requestDelay :: Integral a => RateLimit -> POSIXTime -> Maybe a
    requestDelay (RateLimit r t) now =
      if r == 0 && now < t
        then Just $ round $ (t - now) * 1E6
        else Nothing
    -- merge current RateLimit with the new one. concurrent-safe.
    updateRateLimit Nothing = id
    updateRateLimit x@(Just cur) =
      \case
        Nothing -> x
        y@(Just (RateLimit nr nt))
          | nt == rlReset cur ->
            if nr < rlRemaining cur
              then y
              else x
          | nt < rlReset cur -> x
          | otherwise -> y
    getRateLimit resp =
      RateLimit <$> f "X-RateLimit-Remaining" <*> f "X-RateLimit-Reset"
      where
        f hdr = getHeader resp hdr >>= parseHeader
    getRetryAfter :: HC.Response body -> Maybe NominalDiffTime
    getRetryAfter resp = getHeader resp "Retry-After" >>= parseHeader
    getHeader resp hdr = lookup hdr (HC.responseHeaders resp)
    parseHeader :: (Num c) => C.ByteString -> Maybe c
    parseHeader = fmap fromInteger . readMaybe . C.unpack

httpSource'
  :: (MonadResource m, MonadIO n, MonadReader env m, Has HC.Manager env)
  => HC.Request
  -> (HC.Response (ConduitT () C.ByteString n ()) -> ConduitT () r m ())
  -> ConduitT () r m ()
httpSource' request withRes = do
  env <- ask
  bracketP
    (runReaderT (responseOpen request) env)
    HCC.responseClose
    withRes
  where
    responseOpen req = do
      env <- asks getter
      liftIO $ fmap HCC.bodyReaderSource `fmap` HC.responseOpen req env

-- | Fetch yesterday's daily export for Movie Ids. See: <https://developers.themoviedb.org/3/getting-started/daily-file-exports>.
fetchYdayMovieIdsC ::
     (MonadLogger m, MonadResource m, MonadReader env m, Has HC.Manager env)
  => ConduitT () Id m ()
fetchYdayMovieIdsC = getFileStream .| transPipe liftIO ungzip .| parseIdC
  where
    fmtYdayFileName =
      formatTime defaultTimeLocale "p/exports/movie_ids_%m_%d_%0Y.json.gz" .
      addDays (-1) . utctDay <$>
      getCurrentTime
    getFileStream = do
      path <- liftIO fmtYdayFileName
      req <- liftIO $ HC.parseRequest (tmdbExportURL ++ path)
      logWithoutLoc "" LevelDebug $
        "Getting Movie ids from: " <> show (HC.getUri req)
      httpSource' req $ \resp ->
        if statusIsSuccessful (HC.responseStatus resp)
          then HC.responseBody resp
          else liftIO $ throwIO $ APIException req (APIError Nothing)
    parseIdC = peekForeverE (lineAsciiC (concatMapC getId))
    getId :: C.ByteString -> Maybe Id
    getId bs = do
      res <- decodeStrict' bs
      flip parseMaybe res $ \v -> v .: "id"

-- TODO: Fix it to do paginated requests, currently works fine but the upstream might fix it later.
-- | Fetch Movie Id Changes from endpoint: @\/movie\/changes@
fetchMovieChangesC ::
     ( MonadLoggerIO m
     , MonadReader env m
     , Has HC.Manager env
     , Has RateLimitSettings env
     , Has ApiKey env
     )
  => (Maybe Day, Maybe Day) -- ^ Range of Dates, according to the docs < 14days.
  -> ConduitT () Id m ()
fetchMovieChangesC range = do
  api_key <- asks getter
  let (start, end) = both (T.pack . formatTime defaultTimeLocale "%F" <$>) range
  req <-
    liftIO $
    mkApiRequest
      api_key
      "movie/changes"
      [("start_date", start), ("end_date", end)]
  logWithoutLoc "" LevelDebug $
    "Fetching Movie Changes " <> (maybe mempty (\d -> "from " <> d) start) <>
    (maybe mempty (\d -> " to " <> d) end) <>
    ".."
  resp <- rateLimitedhttpLbsM req
  if statusIsSuccessful $ HC.responseStatus resp
    then CL.sourceList $ fromMaybe [] $ parseIds (HC.responseBody resp)
    else liftIO $ throwIO $ fromAPIError req resp
  where
    parseIds :: LB.ByteString -> Maybe [Id]
    parseIds bs = do
      res <- decode' bs
      flip parseMaybe res $ \v -> do
        r <- v .: "results"
        catMaybes <$> forM r (.:? "id")

-- | Fetch Movies with additional data from endpoint: @\/movie\/{id}@.
fetchMoviesC ::
     ( MonadLoggerIO m
     , MonadReader env m
     , Has HC.Manager env
     , Has RateLimitSettings env
     , Has ApiKey env
     )
  => Int -- ^ Max number of concurrent requests.
  -> ConduitT Id LB.ByteString m ()
fetchMoviesC conc_limit = do
  manager <- asks getter
  settings <- asks getter
  api_key <- asks getter
  logFunc <- askLoggerIO
  let queries =
        [ ( "append_to_response"
          , Just
              (T.intercalate
                 ","
                 ["alternative_titles", "credits", "release_dates"]))
        ]
  sem <- liftIO $ newQSem conc_limit
  out <- liftIO $ newTBQueueIO (fromInteger $ toInteger conc_limit)
  me <- liftIO myThreadId
  awaitForever $ \m_id -- Creates threads to do req and yield their results.
   -> do
    liftIO $ waitQSem sem
    _ <-
      liftIO $
      forkFinally
        (doOne (manager, api_key, settings) m_id queries)
        (either
           (throwTo me)
           (\v -> do
               logFunc defaultLoc "" LevelDebug ("Fetched Movie Id: " <> toLogStr m_id)
               atomically (writeTBQueue out v)
               signalQSem sem))
    yieldTBQueue out
  liftIO $ replicateM_ conc_limit (waitQSem sem) -- Wait for all running threads to terminate.
  yieldTBQueue out -- yield their results.
  where
    yieldTBQueue q =
      liftIO (atomically (flushTBQueue q)) >>= mapM_ (maybe (return ()) yield)
    doOne (manager, api_key, settings) m_id queries = do
      req <- mkApiRequest api_key ("movie/" ++ show m_id) queries
      resp <- rateLimitedhttpLbs settings manager req
      let !r =
            if statusIsSuccessful $ HC.responseStatus resp
              then Just $ HC.responseBody resp
              else case toError resp of
                     Just s -> throwM $ APIException req (APIError s)
                     _      -> Nothing
      return r
    toError resp =
      (\s ->
         if apiStatusCode s == notFoundStatusCode -- notFound responses are not exceptions.
           then Nothing
           else Just s) <$>
      (decode' $ HC.responseBody resp :: Maybe ApiStatus)

-- | Fetch All movie genres from endpoint: @genre\/movie\/list@.
fetchMovieGenres ::
     ( MonadIO m
     , MonadReader env m
     , Has HC.Manager env
     , Has RateLimitSettings env
     , Has ApiKey env
     )
  => m (Maybe Genres)
fetchMovieGenres = do
  api_key <- asks getter
  req <- liftIO $ mkApiRequest api_key "genre/movie/list" []
  resp <- rateLimitedhttpLbsM req
  if statusIsSuccessful $ HC.responseStatus resp
    then return $ decode' (HC.responseBody resp)
    else liftIO $ throwIO $ fromAPIError req resp
