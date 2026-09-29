{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Runner (RunnerInput (..), newRunnerInput, watchTurnBudget)
import Agent.Session (AgentTurn (..))
import Config (defaultAgentConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO (Handle)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (createPipe, fdToHandle)
import System.Process (ProcessHandle, createProcess, proc, waitForProcess)
import System.Timeout (timeout)

main :: IO ()
main = withSystemTempDirectory "turn-watchdog-test" $ \dir -> do
    activeTimeStopsTheTurn dir
    pausedTimeDoesNotStopTheTurn dir

activeTimeStopsTheTurn :: FilePath -> IO ()
activeTimeStopsTheTurn dir = do
    logPath <- writeTurnLog dir "active"
    input <- runnerInput False
    turn <- turnFor logPath
    (_, _, _, ph) <- createProcess (proc "sleep" ["60"])
    started <- getCurrentTime
    _ <- timeout 60000000 (watchTurnBudget defaultAgentConfig turn input ph (2 * 1000000))
    elapsed <- realToFrac . (`diffUTCTime` started) <$> getCurrentTime
    assertBool "an active turn is stopped shortly after its budget" (elapsed < 40)
    assertBool "an active turn is not stopped before its budget" (elapsed >= 2)
    logged <- TIO.readFile logPath
    assertContains "the timeout is recorded in the turn log" "Runner timed out" logged
    alive <- processAlive ph
    assertBool "the runner process is gone" (not alive)

pausedTimeDoesNotStopTheTurn :: FilePath -> IO ()
pausedTimeDoesNotStopTheTurn dir = do
    logPath <- writeTurnLog dir "paused"
    input <- runnerInput True
    turn <- turnFor logPath
    (_, _, _, ph) <- createProcess (proc "sleep" ["60"])
    _ <- forkIO (threadDelay 4000000 >> resumeInput input)
    started <- getCurrentTime
    _ <- timeout 60000000 (watchTurnBudget defaultAgentConfig turn input ph (2 * 1000000))
    elapsed <- realToFrac . (`diffUTCTime` started) <$> getCurrentTime
    assertBool "a paused turn is not stopped while it waits for the user" (elapsed >= 4)
    logged <- TIO.readFile logPath
    assertContains "the timeout is recorded after the pause ends" "Runner timed out" logged

resumeInput :: MVar.MVar (Maybe RunnerInput) -> IO ()
resumeInput input =
    MVar.modifyMVar_ input (return . fmap (\control -> control{inputAwaitingUser = False}))

runnerInput :: Bool -> IO (MVar.MVar (Maybe RunnerInput))
runnerInput awaiting = do
    handle <- unusedHandle
    MVar.newMVar
        ( Just
            RunnerInput
                { inputHandle = handle
                , inputPending = Nothing
                , inputWaiting = []
                , inputPromptSeen = True
                , inputRetrying = False
                , inputQuestion = Nothing
                , inputAwaitingUser = awaiting
                }
        )

unusedHandle :: IO Handle
unusedHandle = do
    (readFd, _) <- createPipe
    fdToHandle readFd

writeTurnLog :: FilePath -> String -> IO FilePath
writeTurnLog dir name = do
    let path = dir ++ "/" ++ name ++ ".log"
    TIO.writeFile path ""
    return path

turnFor :: FilePath -> IO AgentTurn
turnFor logPath = do
    now <- getCurrentTime
    return
        AgentTurn
            { turnId = "watchdog-turn"
            , turnSessionId = "watchdog-session"
            , turnPrompt = "watchdog"
            , turnStatus = "running"
            , turnExitCode = Nothing
            , turnStartedAt = now
            , turnFinishedAt = Nothing
            , turnLogPath = logPath
            , turnLog = ""
            }

processAlive :: ProcessHandle -> IO Bool
processAlive ph = do
    exited <- timeout 1000000 (waitForProcess ph)
    return (exited == Nothing)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertContains :: String -> String -> T.Text -> IO ()
assertContains label needle haystack =
    unless (T.pack needle `T.isInfixOf` haystack) (fail (label ++ ": missing " ++ show needle))
