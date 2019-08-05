{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}

-- |
-- Provides High level functions as needed for the @tmdb-dumper@ program to update\/query DB from [TheMovieDB](https://themoviedb.org) API.
module TMDBDump
  ( ApiKey
  -- * RateLimit Settings
  , RateLimitSettings
  , mkRateLimitSettings
  , minRetryDelay
  , maxRetries
  -- * Database
  , DBVersion
  , UpdateStatus(..)
  , DBInfo
  , dbVersion
  , dbMovieUpdate
  , dbMovieRecords
  -- * Settings.
  , Settings
  , defaultSettings
  , logVerbosity
  , logNoTerminal
  , staleUpdateThresold
  , concurrentRequestLimit
  , insertBatchLimit
  , apiKey
  , dbPath
  , rateLimitSettings
  , httpManagerSettings
  -- * Core Definitions
  , TMDBDumper
  , runTMDBDumper
  , getDBInfo
  , updateMovies
  ) where

import           Conduit
import           Control.Concurrent.MVar
import           Control.Exception.Safe
import           Control.Monad.Extra
import           Control.Monad.Logger     hiding (runStderrLoggingT,
                                           runStdoutLoggingT)
import           Control.Monad.Reader
import qualified Data.Conduit.List        as CL
import qualified Data.Sequence            as Seq
import           Data.Time.Calendar       (Day, addDays, diffDays)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX    (getCurrentTime,
                                           posixSecondsToUTCTime)
import           Data.Tuple.Extra         (both)
import           Database.SQLite.Simple
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           TMDBDump.Internal.Api
import           TMDBDump.Internal.DB     (DBInfo (..), DBVersion (..),
                                           UpdateStatus (..))
import qualified TMDBDump.Internal.DB     as DB
import           TMDBDump.Internal.Logger

-- | Core settings required for actions in 'TMDBDumper'. Please use 'defaultSettings' and modify individual settings.
data Settings = Settings
  { logVerbosity           :: Int                -- ^ Verbosity of log output.
  , logNoTerminal          :: Bool               -- ^ Disable terminal support(No color/sticky) for log output.
  , staleUpdateThresold    :: Int                -- ^ Maximum stale-age of data before we fetch everything.
  , concurrentRequestLimit :: Int                -- ^ Max concurrent TMDB API requests.
  , insertBatchLimit       :: Int                -- ^ Batch limit for sqlite inserts.
  , apiKey                 :: ApiKey             -- ^ TMDB Api Key. See <https://www.themoviedb.org/settings/api> to get one.
  , dbPath                 :: String             -- ^ SQLite DB Path.
  , rateLimitSettings      :: RateLimitSettings  -- ^ RateLimit Settings for TMDB Requests.
  , httpManagerSettings    :: ManagerSettings    -- ^ http settings for requests. See 'tlsManagerSettings' to modify this.
  }

toLogLevel :: Int -> LogLevel
toLogLevel n
  | n >= 2 = LevelDebug
  | n == 1 = LevelInfo
  | otherwise = LevelWarn

-- | Default value for 'Settings'.
defaultSettings :: Settings
defaultSettings =
  Settings 0 False 60 8 40 "" ":memory:" (mkRateLimitSettings 0.1 20) tlsManagerSettings

-- | Official docs say the change date range < 14.
tmdbChangesRequestDiff :: Integer
tmdbChangesRequestDiff = 14

-- | Result type for actions. See 'runTMDBDumper'.
newtype TMDBDumper a =
  TMDBDumper
    { getDumper :: ReaderT (Connection, Manager, Settings) (LoggingT (ResourceT IO)) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadResource
           , MonadLogger
           , MonadLoggerIO
           , MonadReader (Connection, Manager, Settings)
           )

instance MonadUnliftIO TMDBDumper where
  withRunInIO runner = TMDBDumper $ withRunInIO (\f -> runner (f . getDumper))

-- | Execute the action with given settings.
runTMDBDumper :: Settings     -- ^ Dumper settings.
              -> TMDBDumper a -- ^ The action to run.
              -> IO a         -- ^ Result. __May throw exceptions__.
runTMDBDumper settings action = do
  manager <- liftIO $ newTlsManagerWith (httpManagerSettings settings)
  bracket (open (dbPath settings)) close $ \conn -> do
    DB.initDB conn
    runResourceT $
      runStdoutLoggingT
        (toLogLevel $ logVerbosity settings)
        (logNoTerminal settings) $
      flip runReaderT (conn, manager, settings) $ getDumper action

-- | Get 'DBInfo' of the database.
getDBInfo :: TMDBDumper DBInfo
getDBInfo = do
  (conn, _, _) <- ask
  liftIO $ DB.getDBInfo conn

-- | updateMovies @force@ updates movie information in DB by fetching data from TMDB api.
updateMovies :: Bool -> TMDBDumper ()
updateMovies force = do
  (conn, manager, settings) <- ask
  flip runReaderT (manager, rateLimitSettings settings, apiKey settings) $ do
    logInfoN "Updating Movie genres.."
    fetchMovieGenres >>= mapM_ (liftIO . (`DB.updateMovieGenres` conn)) -- Update movie genres.
    logInfoN "Done."
    logInfoN "Updating Movie records.."
    state <- liftIO $ newMVar $ (0 :: Int, Seq.empty) -- state ~ (counter, batch)
    logStickyN "Updated 0 Movie records"
    flip finally (cleanUp state conn) $
      runConduit $ do
        flip DB.updateMovies conn $ \status ->
          sourceC conn status (toInteger $ staleUpdateThresold settings) .|
          fetchMoviesC (concurrentRequestLimit settings) .|
          mapM_C
            (\bs ->
               let batchLimit = insertBatchLimit settings
                in withRunInIO $ \run -> do
                     modifyMVar_
                       state
                       (\(!cnt, !bs_seq) ->
                          if Seq.length bs_seq == batchLimit
                            then do
                              let !cnt' = cnt + batchLimit
                              DB.insertMovies bs_seq conn
                              run $
                                logSticky "" $
                                "Updated " <> toLogStr cnt' <> " Movie records"
                              return (cnt', Seq.empty)
                            else let !nbs = (Seq.|>) bs_seq bs -- force apply.
                                  in return (cnt, nbs)))
    logInfoN "Movie table update complete."
  where
    -- | Flush batch to DB and refresh log.
    cleanUp state conn = do
      (n, s) <- liftIO $ readMVar state
      liftIO $ DB.insertMovies s conn
      logStickyDone "" $
        "Updated " <> toLogStr (n + Seq.length s) <> " Movie records"
    -- | Source Conduit for Movie Ids.
    sourceC conn status upd_thresold = do
      td <- liftIO $ utctDay <$> getCurrentTime
      let fetchAllC -- Fetch all Ids.
           =
            fetchYdayMovieIdsC *>
            fetchMovieChangesC (Just $ addDays (-1) td, Nothing)
          splitChangesC start -- Fetch all id changes starting from start date.
           =
            sequence_
              (fetchMovieChangesC . both Just <$> splitPeriod (start, td)) *>
            fetchMovieChangesC (Just td, Nothing)
      if force
        then fetchAllC
        else case status of
               DB.UpdateNull -> fetchAllC
               DB.UpdatePartial t ->
                 let ld = utctDay $ posixSecondsToUTCTime t
                     diff = diffDays td ld
                  in if diff > upd_thresold
                       then do
                         logWithoutLoc "" LevelInfo $
                           "Partial update older than " <> toLogStr upd_thresold <>
                           ", Re-fetching everything.."
                         fetchAllC
                       else (fetchYdayMovieIdsC .|
                             filterMC (liftIO . notM . flip DB.hasMovie conn)) *>
                            if diff <= 0
                              then fetchMovieChangesC
                                     (Just $ addDays (-1) td, Nothing)
                              else splitChangesC ld
               DB.UpdateComplete t ->
                 let ld = utctDay $ posixSecondsToUTCTime t
                     diff = diffDays td ld
                  in if | diff < 0 ->
                          logWarnN "Invalid update timestamp" >> CL.sourceNull
                        | diff == 0 -> fetchMovieChangesC (Just td, Nothing)
                        | diff > upd_thresold ->
                          do logWithoutLoc "" LevelInfo $
                               "Last Update older than " <>
                               toLogStr upd_thresold <>
                               ", Re-fetching everything.."
                             fetchAllC
                        | otherwise -> splitChangesC ld
    -- split the given range to be max tmdbChangesRequestDiff apart.
    splitPeriod :: (Day, Day) -> [(Day, Day)]
    splitPeriod (x, y)
      | diff <= 0 = error "Assertion: start_date < end_date"
      | diff <= tmdbChangesRequestDiff = [(x, y)]
      | otherwise =
        let r = diff `mod` tmdbChangesRequestDiff
         in (zip <*> tail) $
            [x,(addDays tmdbChangesRequestDiff x) .. (addDays (-r) y)] ++
            (if r == 0
               then []
               else [y])
      where
        diff = diffDays y x
