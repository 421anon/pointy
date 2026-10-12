{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Runner (RunnerInput, handleDialog, newRunnerInput, planSteer, watchTurnBudget)
import Agent.Session (AgentTurn (..))
import Config (defaultAgentConfig)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Process (ProcessHandle, createProcess, proc, waitForProcess)
import System.Timeout (timeout)

main :: IO ()
main = withSystemTempDirectory "turn-watchdog-test" $ \dir -> do
    activeTimeStopsTheTurn dir
    openQuestionDoesNotStopTheTurn dir

activeTimeStopsTheTurn :: FilePath -> IO ()
activeTimeStopsTheTurn dir = do
    logPath <- writeTurnLog dir "active"
    input <- runnerInput
    (abortedAfter, ph) <- watchSleeper logPath input
    assertBool ("an active turn is aborted once its budget is spent, aborted after " ++ show abortedAfter) (abortedAfter >= 2 && abortedAfter < 4)
    alive <- processAlive ph
    assertBool "the runner process is gone" (not alive)

openQuestionDoesNotStopTheTurn :: FilePath -> IO ()
openQuestionDoesNotStopTheTurn dir = do
    logPath <- writeTurnLog dir "question"
    input <- runnerInput
    handleDialog defaultAgentConfig logPath input (selectEvent "d1")
    _ <- forkIO (threadDelay 4000000 >> answerQuestion input)
    (abortedAfter, _) <- watchSleeper logPath input
    assertBool ("time waiting for the user's answer is not charged, aborted after " ++ show abortedAfter) (abortedAfter >= 4.5)

watchSleeper :: FilePath -> MVar (Maybe RunnerInput) -> IO (Double, ProcessHandle)
watchSleeper logPath input = do
    (_, _, _, ph) <- createProcess (proc "sleep" ["60"])
    started <- getCurrentTime
    aborted <- newEmptyMVar
    _ <- forkIO (awaitTimeoutLine started aborted)
    _ <- timeout 60000000 (watchTurnBudget defaultAgentConfig (turnFor logPath started) input ph (2 * 1000000))
    abortedAfter <- takeMVar aborted
    return (abortedAfter, ph)
  where
    awaitTimeoutLine started aborted = do
        logged <- TIO.readFile logPath
        if "Runner timed out" `T.isInfixOf` logged
            then getCurrentTime >>= putMVar aborted . realToFrac . (`diffUTCTime` started)
            else threadDelay 50000 >> awaitTimeoutLine started aborted
    turnFor path now =
        AgentTurn
            { turnId = "watchdog-turn"
            , turnSessionId = "watchdog-session"
            , turnPrompt = "watchdog"
            , turnAutomatic = False
            , turnStatus = "running"
            , turnExitCode = Nothing
            , turnStartedAt = now
            , turnFinishedAt = Nothing
            , turnLogPath = path
            , turnLog = ""
            }

answerQuestion :: MVar (Maybe RunnerInput) -> IO ()
answerQuestion input = do
    reply <- newEmptyTMVarIO
    modifyMVar_ input $ \current ->
        return (current >>= fmap (\(_, answered, _) -> answered) . planSteer "1" "answer" reply)

runnerInput :: IO (MVar (Maybe RunnerInput))
runnerInput = do
    (readFd, writeFd) <- createPipe
    closeFd readFd
    newRunnerInput =<< fdToHandle writeFd

selectEvent :: Text -> Value
selectEvent dialogId =
    object
        [ "type" .= ("extension_ui_request" :: Text)
        , "id" .= dialogId
        , "method" .= ("select" :: Text)
        , "title" .= ("Which layout?" :: Text)
        , "options" .= (["1. Card", "2. Row", "3. Type something."] :: [Text])
        ]

writeTurnLog :: FilePath -> String -> IO FilePath
writeTurnLog dir name = do
    let path = dir ++ "/" ++ name ++ ".log"
    TIO.writeFile path ""
    return path

processAlive :: ProcessHandle -> IO Bool
processAlive ph = do
    exited <- timeout 1000000 (waitForProcess ph)
    return (exited == Nothing)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)
