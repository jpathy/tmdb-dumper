module TMDBDump.Internal.DB
  ( DBVersion(..)
  , currentDBVersion
  , getDBVersion
  , DBInfo(..)
  , getDBInfo
  , UpdateStatus(..)
  , getLastMovieUpdate
  , initDB
  , hasMovie
  , insertMovies
  , updateMovieGenres
  , updateMovies
  ) where

import           Control.Monad           (when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Text         (encodeToLazyText)
import qualified Data.ByteString.Lazy    as LB
import           Data.Either.Extra       (eitherToMaybe)
import           Data.Foldable           (forM_)
import           Data.Maybe              (isNothing)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time.Clock         (utctDay)
import           Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime,
                                          posixSecondsToUTCTime)
import           Database.SQLite.Simple
import           TMDBDump.Internal.Types

-- | Version stored in sqlite `user_version` to track schema changes.
data DBVersion = V1 deriving (Show, Eq)

versionFromInt :: Int -> Maybe DBVersion
versionFromInt 1 = Just V1
versionFromInt _ = Nothing

versionToInt :: DBVersion -> Int
versionToInt V1 = 1

-- | Latest Database version supported.
currentDBVersion :: DBVersion
currentDBVersion = V1

-- | Get DB Version.
getDBVersion :: Connection -> IO (Maybe DBVersion)
getDBVersion conn = do
  [Only x] <- query_ conn "PRAGMA user_version"
  return $ versionFromInt x

-- | Maximum Busy timeout in milliseconds for sqlite DB. See: https://www.sqlite.org/c3ref/busy_timeout.html.
-- | We set this to avoid SQLITE_BUSY while info operation takes place.
maxBusyTimeout :: Int
maxBusyTimeout = 10000

-- | Intialize database with tables/convert from an old version.
initDB :: Connection -> IO ()
initDB conn = do
  -- We set a busy timeout for all connections.
  execute_ conn $
    Query $
    T.pack $ "PRAGMA busy_timeout=" ++ show (maxBusyTimeout)
  ver <- getDBVersion conn
  when (isNothing ver) $ do
    execute_ conn "DROP TABLE IF EXISTS '.metadata'"
    execute_ conn "DROP TABLE IF EXISTS movie_genres"
    execute_ conn "DROP TABLE IF EXISTS movies"
  when (isNothing ver) initCurrent -- For future versions we may want to convert the DB here.
  where
    initCurrent = do
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS '.metadata' (last_updated_movies INTEGER)"
      execute_
        conn "CREATE TABLE IF NOT EXISTS movie_genres (id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS movies (tmdb_id INTEGER PRIMARY KEY, title TEXT NOT NULL, original_title TEXT, original_language TEXT, status TEXT, release_date TEXT, imdb_id TEXT, adult BOOLEAN, genre_ids TEXT, spoken_languages TEXT, production_countries TEXT, runtime INTEGER, budget INTEGER, revenue INTEGER, json_response TEXT NOT NULL)"
      execute_ conn $
        Query $
        T.pack $ "PRAGMA user_version=" ++ show (versionToInt currentDBVersion)

-- | Update genres in the database.
updateMovieGenres :: Genres -> Connection -> IO ()
updateMovieGenres (Genres gs) conn =
  withImmediateTransaction conn $
  forM_ gs $ \g ->
    execute
      conn
      "INSERT OR REPLACE INTO movie_genres(id, name) VALUES(?, ?)"
      (genreId g, genreName g)

-- | Last Update Status of any table. Conservative approximation of age of data.
data UpdateStatus
  = UpdateNull               -- ^ Update not initiated.
  | UpdatePartial POSIXTime  -- ^ Partial update started at some unix epoch.
  | UpdateComplete POSIXTime -- ^ Successful completion of update at some unix epoch.
  deriving (Eq)

instance Show UpdateStatus where
  show UpdateNull = "None"
  show (UpdatePartial t) =
    "Incomplete, started on: " ++ show (utctDay $ posixSecondsToUTCTime t)
  show (UpdateComplete t) = show $ utctDay $ posixSecondsToUTCTime t

-- | Get Last update status of movies table in the DB.
getLastMovieUpdate :: Connection -> IO UpdateStatus
getLastMovieUpdate conn = do
  xs <-
    query_ conn "SELECT last_updated_movies FROM '.metadata' WHERE ROWID = 1"
  return $
    case xs of
      [Only x] -> parseTS x
      _        -> UpdateNull
  where
    parseTS :: Maybe Int -> UpdateStatus
    parseTS Nothing = UpdateNull
    parseTS (Just n) =
      let t = fromInteger $ toInteger $ abs n
       in if n < 0
            then UpdatePartial t
            else UpdateComplete t

-- | Wrap the given update function tracking the update status.
updateMovies :: MonadIO m => (UpdateStatus -> m ()) -> Connection -> m ()
updateMovies f conn = do
  lu <- liftIO $ getLastMovieUpdate conn
  st <- liftIO (round <$> getPOSIXTime :: IO Int)
  -- Transition: UpdateNull -> UpdatePartial x -> UpdateComplete y
  -- Only record the timestamp of first partial update. (conservative)
  when (lu == UpdateNull) $
    liftIO $
    execute
      conn
      "INSERT OR REPLACE INTO '.metadata'(ROWID, last_updated_movies) VALUES(1, ?)" $
    Only (-st)
  f lu  -- Exception thrown here means unsuccessful update.
  liftIO $
    execute conn "UPDATE '.metadata' SET last_updated_movies=? WHERE ROWID = 1" $
    Only st

-- | Check if the movie table already has an entry for the given id.
hasMovie :: Int -> Connection -> IO Bool
hasMovie m_id conn = do
  [Only b] <-
    query
      conn
      "SELECT EXISTS(SELECT tmdb_id FROM movies WHERE tmdb_id = ?)"
      (Only m_id)
  return b

-- | Insert/update movie records in DB by parsing the response bodies.
insertMovies ::
     (MonadLoggerIO m, Traversable t)
  => t (Int, LB.ByteString)
  -> Connection
  -> m ()
insertMovies inp conn = do
  let res = -- map ByteString to Maybe (Movie, ByteString)
        (fmap . fmap)
          (\s ->
             (,) <$> decode' s <*>
             (eitherToMaybe . TE.decodeUtf8' . LB.toStrict) s)
          inp
  logFn <- askLoggerIO
  liftIO $
    withImmediateTransaction conn $
    forM_
      res
      (\(m_id, v) ->
         case v of
           Nothing ->
             logFn
               defaultLoc
               ""
               LevelWarn
               ("Failed to decode response for movie id: " <> toLogStr m_id)
           Just v' -> uncurry insert v')
  where
    insert :: Movie -> T.Text -> IO ()
    insert m json_response =
      executeNamed
        conn
        "INSERT OR REPLACE INTO movies(tmdb_id, title, original_title, original_language, status, release_date, imdb_id, adult, genre_ids, spoken_languages, production_countries, runtime, budget, revenue, json_response) VALUES (:tmdb_id, :title, :otitle, :olang, :status, :release_date, :imdb_id, :adult, :genres, :spoken_langs, :prod_countries, :runtime, :budget, :revenue, :json_response)"
        [ ":tmdb_id" := movieId m
        , ":title" := movieTitle m
        , ":otitle" := movieOriginalTitle m
        , ":olang" := movieOriginalLanguage m
        , ":status" := movieStatus m
        , ":release_date" := movieReleaseDate m
        , ":imdb_id" := movieIMDBId m
        , ":adult" := movieIsAdult m
        , ":genres" := encodeToLazyText (genreId <$> movieGenres m)
        , ":spoken_langs" :=
          encodeToLazyText ((\(x, y) -> [x, y]) <$> movieSpokenLanguages m)
        , ":prod_countries" :=
          encodeToLazyText ((\(x, y) -> [x, y]) <$> movieProductionCountries m)
        , ":runtime" := movieRuntime m
        , ":budget" := movieBudget m
        , ":revenue" := movieRevenue m
        , ":json_response" := json_response
        ]

-- | Database Information.
data DBInfo = DBInfo
  { dbVersion      :: Maybe DBVersion -- ^ DB Version.
  , dbMovieUpdate  :: UpdateStatus    -- ^ Update status of Movie table.
  , dbMovieRecords :: Int             -- ^ No. of records in Movie table.
  }
  deriving (Eq, Show)

-- | Get DBInfo.
getDBInfo :: Connection -> IO DBInfo
getDBInfo conn =
  handle
    (\(e :: SQLError) ->
       case (sqlError e) of
         ErrorError -> error "Not a valid database."
         _          -> throwIO e) $ do
    ver <- getDBVersion conn
    mu <- getLastMovieUpdate conn
    [Only mc] <- query_ conn "SELECT COUNT(*) FROM movies"
    return $ DBInfo ver mu mc
