-- |
-- Ripped from RIO.Prelude.Logger to do colorful/sticky logging, when possible.
module TMDBDump.Internal.Logger
  ( runStderrLoggingT
  , runStdoutLoggingT
  , logSticky
  , logStickyDone
  , logStickyN
  , logStickyDoneN
  ) where
import           Control.Concurrent.MVar
import           Control.Exception       (bracket)
import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger    hiding (runStderrLoggingT,
                                          runStdoutLoggingT)
import qualified Data.ByteString.Char8   as S8
import qualified Data.Text               as T
import           Data.Time.Calendar      (Day (..))
import           Data.Time.Clock         (UTCTime (..), getCurrentTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import           System.IO               (Handle, hIsTerminalDevice, stderr,
                                          stdout)
import           System.Log.FastLogger   (fromLogStr)

data LogOptions = LogOptions
  { logMinLevel :: !LogLevel
  , logTerminal :: !Bool
  , logUseTime  :: !Bool
  , logUseColor :: !Bool
  , logSend     :: !(S8.ByteString -> IO ())
  }

setLogNoTerminal :: LogOptions -> LogOptions
setLogNoTerminal options = options {logTerminal = False, logUseColor = False}

logOptionsHandle
  :: MonadIO m
  => Handle
  -> m LogOptions
logOptionsHandle handle' =
  liftIO $ do
    terminal <- hIsTerminalDevice handle'
    return
      LogOptions
        { logMinLevel = LevelDebug
        , logTerminal = terminal
        , logUseTime  = True
        , logUseColor = terminal
        , logSend = S8.hPutStr handle'
        }

-- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
-- This definition is top-level in order to avoid multiple reevaluation at runtime.
timestampLength :: Int
timestampLength =
  length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

noSticky :: LogLevel -> LogLevel
noSticky (LevelOther "sticky-done") = LevelInfo
noSticky (LevelOther "sticky")      = LevelDebug
noSticky level                      = level

runWithOptionsLoggingT :: MonadUnliftIO m => LogOptions -> LoggingT m a -> m a
runWithOptionsLoggingT options lma =
  if logTerminal options
    then withRunInIO $ \run ->
           bracket
             (newMVar mempty)
             (\var -> do
                state <- takeMVar var
                unless (S8.null state) (logSend options "\n"))
             (\var ->
                run $
                runLoggingT lma $ stickyImpl var options (simpleLogFunc options))
    else (runLoggingT lma $ \loc src lvl msg ->
            simpleLogFunc options loc src (noSticky lvl) msg)

runHLoggingT :: MonadUnliftIO m => Handle -> LogLevel -> Bool -> LoggingT m a -> m a
runHLoggingT handle minLevel noTerm lma = do
  options <- logOptionsHandle handle
  flip runWithOptionsLoggingT lma $
    (if noTerm
       then setLogNoTerminal
       else id)
      options {logMinLevel = minLevel}

runStdoutLoggingT :: MonadUnliftIO m => LogLevel -> Bool -> LoggingT m a -> m a
runStdoutLoggingT = runHLoggingT stdout

runStderrLoggingT :: MonadUnliftIO m => LogLevel -> Bool -> LoggingT m a -> m a
runStderrLoggingT = runHLoggingT stderr

logSticky :: (MonadLogger m, ToLogStr msg) => LogSource -> msg -> m ()
logSticky src msg = monadLoggerLog defaultLoc src (LevelOther "sticky") msg

logStickyDone :: (MonadLogger m, ToLogStr msg) => LogSource -> msg -> m ()
logStickyDone src msg = monadLoggerLog defaultLoc src (LevelOther "sticky-done") msg

logStickyN :: MonadLogger m => T.Text -> m ()
logStickyN = logSticky ""

logStickyDoneN :: MonadLogger m => T.Text -> m ()
logStickyDoneN = logStickyDone ""

simpleLogFunc :: LogOptions
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
simpleLogFunc lo loc src level msg =
  when (level >= logMinLevel lo) $ do
    timestamp <- getTimestamp
    logSend lo $
      fromLogStr $
      timestamp <> getLevelSrc <> ansi reset <> msg <> getLoc <> ansi reset <> "\n"
  where
    reset = "\ESC[0m"
    setBlack = "\ESC[90m"
    setGreen = "\ESC[32m"
    setBlue = "\ESC[34m"
    setYellow = "\ESC[33m"
    setRed = "\ESC[31m"
    setMagenta = "\ESC[35m"
    ansi :: LogStr -> LogStr
    ansi xs
      | logUseColor lo = xs
      | otherwise = mempty
    getTimestamp :: IO LogStr
    getTimestamp
      | logUseTime lo = do
        now <- getCurrentTime
        return $ ansi setBlack <> toLogStr (formatTime' now) <> ": "
      | otherwise = return mempty
      where
        formatTime' =
          take timestampLength . formatTime defaultTimeLocale "%F %T.%q"
    getLevelSrc :: LogStr
    getLevelSrc =
      (case level of
        LevelDebug      -> ansi setGreen <> "[debug"
        LevelInfo       -> ansi setBlue <> "[info"
        LevelWarn       -> ansi setYellow <> "[warn"
        LevelError      -> ansi setRed <> "[error"
        LevelOther name -> ansi setMagenta <> "[" <> toLogStr name
       ) <> (if T.null src
              then mempty
              else "#" <> toLogStr src) <> "] "
    getLoc :: LogStr
    getLoc = if (loc == defaultLoc) then mempty else ansi setBlack <> "\n@(" <> toLogStr (S8.pack fileLocStr) <> ")"
    -- taken from file-location package turn the TH Loc loaction information
    -- into a human readable string leaving out the loc_end parameter
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

stickyImpl :: MVar S8.ByteString -> LogOptions
           -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
           -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
stickyImpl ref lo logFn loc src level msgOrig = modifyMVar_ ref $ \sticky -> do
  let backSpaceChar = '\8'
      repeating = mconcat . replicate (S8.length sticky) . S8.singleton
      clear = logSend lo
        (repeating backSpaceChar <>
        repeating ' ' <>
        repeating backSpaceChar)
  case level of
    LevelOther "sticky-done" -> do
      clear
      logFn loc src LevelInfo msgOrig
      return mempty
    LevelOther "sticky" -> do
      clear
      let bs = fromLogStr msgOrig
      logSend lo bs
      return bs
    _
      | level >= logMinLevel lo -> do
          clear
          logFn loc src level msgOrig
          unless (S8.null sticky) $ logSend lo sticky
          return sticky
      | otherwise -> return sticky
