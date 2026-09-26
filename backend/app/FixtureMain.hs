{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import App (runServer)
import Config (loadConfig)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fixture.Document (FixtureDocument (..), loadDocument)
import Fixture.Server (fixtureApp)
import Interpreters.Fixture (newFixtureState, resetFixture, runFixture)
import Processes (cli)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.Environment (getArgs, setEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import UserRepo (ensureUserRepo)

data Options = Options
    { optionRepoSource :: FilePath
    , optionState :: FilePath
    , optionFrontend :: FilePath
    , optionPort :: Int
    , optionWorkDir :: FilePath
    }

parseOptions :: [String] -> Either String Options
parseOptions = go (Options "" "" "" 8081 "")
  where
    go options [] = Right options
    go options ("--repo-source" : value : rest) = go options{optionRepoSource = value} rest
    go options ("--state" : value : rest) = go options{optionState = value} rest
    go options ("--frontend" : value : rest) = go options{optionFrontend = value} rest
    go options ("--work-dir" : value : rest) = go options{optionWorkDir = value} rest
    go options ("--port" : value : rest) = go options{optionPort = read value} rest
    go _ (unknown : _) = Left ("unknown argument: " ++ unknown)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    options <- either (\err -> putStrLn err >> exitFailure) pure (parseOptions arguments)
    when (null (optionRepoSource options) || null (optionState options) || null (optionFrontend options) || null (optionWorkDir options)) $
        putStrLn "usage: pointy-fixture-server --repo-source DIR --state FILE --frontend DIR --work-dir DIR [--port N]" >> exitFailure
    run options

run :: Options -> IO ()
run options = do
    let workDir = optionWorkDir options
        home = workDir </> "home"
        origin = workDir </> "origin.git"
        configPath = workDir </> "config.toml"
        keyfile = workDir </> "key"
    createDirectoryIfMissing True home
    setEnv "HOME" home
    setEnv "XDG_CACHE_HOME" (workDir </> "cache")
    setEnv "GIT_CONFIG_GLOBAL" (workDir </> "gitconfig")
    writeFile (workDir </> "gitconfig") "[safe]\n\tdirectory = *\n"
    writeFile keyfile ""
    document <- either (\err -> error ("fixture document: " ++ err)) pure =<< loadDocument (optionState options)
    removePath origin
    removePath (home </> "user-repo.git")
    removePath (home </> "user-repo.lock")
    cloneRepo (optionRepoSource options) origin
    writeConfig configPath origin keyfile (documentBranch document)
    setEnv "POINTY_CONFIG_PATH" configPath
    config <- loadConfig configPath
    setCurrentDirectory workDir
    ensureUserRepo config
    state <- newFixtureState document
    let reset = do
            resetFixture state
            removePath origin
            cloneRepo (optionRepoSource options) origin
            removePath (home </> "user-repo.git")
            removePath (home </> "user-repo.lock")
            ensureUserRepo config
    putStrLn ("Fixture server on port " ++ show (optionPort options))
    runServer
        (runFixture state)
        (fixtureApp state (optionFrontend options) reset)
        (optionPort options)
        (putStrLn "Fixture server listening.")

writeConfig :: FilePath -> FilePath -> FilePath -> T.Text -> IO ()
writeConfig configPath origin keyfile branch =
    TIO.writeFile configPath $
        T.unlines
            [ "[user-repo]"
            , "url = \"file://" <> T.pack origin <> "\""
            , "keyfile = \"" <> T.pack keyfile <> "\""
            , "branch = \"" <> branch <> "\""
            ]

cloneRepo :: FilePath -> FilePath -> IO ()
cloneRepo source target = do
    outcome <- cli "git" ["clone", "--bare", source, target]
    case outcome of
        (ExitSuccess, _, _) -> pure ()
        (ExitFailure code, _, err) -> error ("git clone --bare " ++ source ++ " failed (exit " ++ show code ++ "): " ++ err)

removePath :: FilePath -> IO ()
removePath path = do
    outcome <- try (removeDirectoryRecursive path)
    case outcome of
        Right () -> pure ()
        Left (_ :: SomeException) -> do
            fileOutcome <- try (removeFile path)
            case fileOutcome of
                Right () -> pure ()
                Left (_ :: SomeException) -> pure ()
