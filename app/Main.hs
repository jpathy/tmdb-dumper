{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Control.Monad       (forM_)
import           Data.List           (nub)
import qualified Data.Text           as T
import           Data.Version
import           Options.Applicative
import           Paths_tmdb_dumper   (version)
import           System.Environment
import           System.Exit
import           TMDBDump

-- | update command uption.
data UpdateOption = MovieOption
  deriving (Bounded, Enum, Eq, Show)

-- | Command for argument.
data OptCommand
  = OptUpdate Bool [UpdateOption]
  | OptInfo
  deriving (Show)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

argsParser :: ParserInfo (Int, Bool, OptCommand, String)
argsParser =
  info
    (versionP <*> helper <*>
     (uncurry3 (,,,) <$> parseOptions <*>
      (strArgument (metavar "FILE") :: Parser String)))
    (header "tmdb-dumper: A tool to dump data from TheMovieDB")
  where
    parseOptions :: Parser (Int, Bool, OptCommand)
    parseOptions =
      hsubparser
        (command
           "update"
           (info
              (withCommonOpts <*> updateP)
              (progDesc "Update/Create the database FILE")) <>
         command
           "info"
           (info
              (withCommonOpts <*> (pure OptInfo))
              (progDesc "Show metadata information for the database FILE")))
      where
        withCommonOpts = (,,) <$> verboseP <*> isNoTermP
        updateP =
          OptUpdate <$>
          switch
            (short 'f' <> long "force" <>
             help "Force updating all records ignoring existing data") <*>
          ((nub <$>
            some
              (flag'
                 MovieOption
                 (short 'm' <>
                  help "Update movies in database FILE from TheMovieDB"))) <|>
           -- '-a' implies update everything.
           flag'
             [minBound .. maxBound]
             (short 'a' <>
              help "Update Everything in database FILE from TheMovieDB"))
    versionP :: Parser (a -> a)
    versionP =
      abortOption (InfoMsg $ showVersion version) $
      mconcat [long "Version", short 'V', help "Show current version", hidden]
    verboseP :: Parser Int
    verboseP =
      length <$>
      many
        (flag'
           ()
           (short 'v' <> help "Verbose output, repeat to increase verbosity"))
    isNoTermP :: Parser Bool
    isNoTermP =
      switch (long "no-terminal" <> help "Disable terminal support for output")

runUpdates :: Bool -> Settings -> [UpdateOption] -> IO ()
runUpdates force settings xs = do
  api_key <-
    handle
      (\(_ :: IOError) ->
         die
           "Missing required environment variable API_KEY. To get an api key follow: https://www.themoviedb.org/settings/api") $
    T.pack <$> getEnv "API_KEY"
  runTMDBDumper settings {apiKey = api_key} $ forM_ xs update
  where
    update MovieOption = updateMovies force

main :: IO ()
main = do
  (verbosity, noTerm, cmd, path) <-
    customExecParser (prefs showHelpOnEmpty) argsParser
  let settings =
        defaultSettings
          {logVerbosity = verbosity, logNoTerminal = noTerm, dbPath = path}
  case cmd of
    OptInfo            -> runTMDBDumper settings getDBInfo >>= fmt
    OptUpdate force us -> runUpdates force settings us
  where
    fmt dbnfo = do
      putStrLn $
        "Version:           " ++ maybe "Uninitialized" show (dbVersion dbnfo)
      putStrLn $ "Last Movie Update: " ++ show (dbMovieUpdate dbnfo)
      putStrLn $ "Movie Records:     " ++ show (dbMovieRecords dbnfo)
