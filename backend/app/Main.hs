{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Git (sweepStaleRunningSessions)
import App (runServer)
import Config (loadConfig, resolveConfigPath)
import Control.Concurrent (forkIO)
import Control.Monad.Except (runExceptT)
import Handlers.ClusterStream (startClusterPoller)
import Handlers.RunStep (restoreJobsFromSlurm)
import Handlers.Statuses (restoreRunningStatuses)
import Interpreters.Production (runProduction)
import OutPaths (warmProjectOutPaths)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import UserRepo (ensureUserRepo, fetchRepo)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Loading configuration..."
    configPath <- resolveConfigPath
    config <- loadConfig configPath
    putStrLn "Ensuring user repo is configured..."
    ensureUserRepo config
    putStrLn "Resetting stale agent runner state..."
    sweepStaleRunningSessions

    putStrLn "Fetching repository updates..."
    fetchResult <- runExceptT fetchRepo
    case fetchResult of
        Left err -> putStrLn $ "Warning: Failed to fetch repository: " ++ err
        Right () -> putStrLn "Repository fetched successfully."

    putStrLn "Starting server on port 8081..."
    runServer runProduction id 8081 warmAfterServerStart
  where
    warmAfterServerStart = do
        putStrLn "Server listening on port 8081."
        _ <- forkIO $ do
            putStrLn "Warming project out paths..."
            runProduction warmProjectOutPaths
            putStrLn "Project out paths warmed."
            putStrLn "Restoring slurm jobs..."
            runProduction restoreJobsFromSlurm
            putStrLn "Slurm jobs restored."
            putStrLn "Restoring running statuses..."
            runProduction restoreRunningStatuses
            putStrLn "Running statuses restored."
        putStrLn "Starting cluster status poller..."
        startClusterPoller
