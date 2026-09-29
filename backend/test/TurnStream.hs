{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Runner (ActiveTool (..), finishActiveTool, startActiveTool, streamLoop, visibleActivity)
import Data.Maybe (isJust)
import Agent.Session (AgentSession (..), AgentTurn (..), saveSession, saveTurn, turnLogFilePath)
import Agent.TurnSignal (registerTurnSignal, signalTurnLog)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO

import Data.Time.Clock (addUTCTime, getCurrentTime)
import Servant.Types.SourceT (StepT (..))
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

main :: IO ()
main = withSystemTempDirectory "turn-stream-test" $ \home -> do
    setEnv "HOME" home
    now <- getCurrentTime
    saveSession
        AgentSession
            { sessionId = "s1"
            , sessionName = Nothing
            , targetBranch = "main"
            , agentBranch = "agent-branch"
            , baseCommit = "abc123"
            , worktreePath = home </> "worktree"
            , status = "open"
            , preparedApply = Nothing
            , activeTurnId = Nothing
            , lastError = Nothing
            , createdAt = now
            , updatedAt = now
            }
    logPath <- turnLogFilePath "s1" "t1"
    let turn =
            AgentTurn
                { turnId = "t1"
                , turnSessionId = "s1"
                , turnPrompt = "hello"
                , turnStatus = "running"
                , turnExitCode = Nothing
                , turnStartedAt = now
                , turnFinishedAt = Nothing
                , turnLogPath = logPath
                , turnLog = ""
                }
    saveTurn turn
    TIO.writeFile logPath ""
    signal <- registerTurnSignal logPath
    let pull0 = streamLoop turn 0 signal

    nothing1 <- timeout 1000000 (pullStep pull0)
    assertEqual "no event without any signal" Nothing (fmap (const ()) nothing1)

    TIO.appendFile logPath "[stdout] first line\n"
    nothing2 <- timeout 1000000 (pullStep pull0)
    assertEqual "no event after append without signal" Nothing (fmap (const ()) nothing2)

    signalTurnLog logPath
    mChunk <- timeout 2000000 (pullStep pull0)
    case mChunk of
        Nothing -> fail "chunk event did not arrive after signal"
        Just Nothing -> fail "stream ended before chunk"
        Just (Just (chunkBytes, pull1)) -> do
            assertBool "chunk event name" ("event: chunk" `BS.isInfixOf` chunkBytes)
            assertBool "chunk carries log line" ("first line" `BS.isInfixOf` chunkBytes)
            let finished = turn{turnStatus = "succeeded", turnExitCode = Just 0, turnFinishedAt = Just now}
            saveTurn finished
            mDone <- timeout 2000000 (pullStep pull1)
            case mDone of
                Nothing -> fail "done event did not arrive after turn save"
                Just Nothing -> fail "stream ended before done"
                Just (Just (doneBytes, pull2)) -> do
                    assertBool "done event name" ("event: done" `BS.isInfixOf` doneBytes)
                    end <- timeout 1000000 (pullStep pull2)
                    case end of
                        Just Nothing -> pure ()
                        Nothing -> fail "stream stayed open after done"
                        Just _ -> fail "stream emitted an event after done"

    activityIsReportedForLongTools home

pullStep :: IO (StepT IO BS.ByteString) -> IO (Maybe (BS.ByteString, IO (StepT IO BS.ByteString)))
pullStep mstep = do
    step <- mstep
    case step of
        Yield bs rest -> do
            pure (Just (bs, pure rest))
        Skip rest -> pullStep (pure rest)
        Effect m -> pullStep m
        Stop -> pure Nothing
        Error e -> fail ("stream error: " ++ show e)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual

activityIsReportedForLongTools :: FilePath -> IO ()
activityIsReportedForLongTools home = do
    now <- getCurrentTime
    saveSession
        AgentSession
            { sessionId = "s2"
            , sessionName = Nothing
            , targetBranch = "main"
            , agentBranch = "agent-branch"
            , baseCommit = "abc123"
            , worktreePath = home </> "worktree2"
            , status = "open"
            , preparedApply = Nothing
            , activeTurnId = Nothing
            , lastError = Nothing
            , createdAt = now
            , updatedAt = now
            }
    logPath <- turnLogFilePath "s2" "t2"
    let turn =
            AgentTurn
                { turnId = "t2"
                , turnSessionId = "s2"
                , turnPrompt = "hello"
                , turnStatus = "running"
                , turnExitCode = Nothing
                , turnStartedAt = now
                , turnFinishedAt = Nothing
                , turnLogPath = logPath
                , turnLog = ""
                }
    saveTurn turn
    TIO.writeFile logPath ""
    signal <- registerTurnSignal logPath
    startedLongAgo <- getCurrentTime
    let tool =
            ActiveTool
                { activeToolId = "call_1"
                , activeToolName = "bash"
                , activeToolText = "grep -rln baseCommit /home"
                , activeToolStartedAt = addUTCTime (-30) startedLongAgo
                }
    startActiveTool logPath "t2" tool
    mFirst <- timeout 8000000 (pullStep (streamLoop turn 0 signal))
    firstPull <- case mFirst of
        Nothing -> fail "stream produced no first event"
        Just Nothing -> fail "stream ended before the first event"
        Just (Just (bytes, pullNext)) -> do
            assertBool ("first event is the heartbeat, got: " ++ show bytes) ("event: heartbeat" `BS.isInfixOf` bytes)
            pure pullNext
    mActivity <- timeout 8000000 (pullStep firstPull)
    activityPull <- case mActivity of
        Nothing -> fail "activity event did not arrive for a long-running tool"
        Just Nothing -> fail "stream ended before the activity event"
        Just (Just (activityBytes, pullAfter)) -> do
            assertBool ("activity event name, got: " ++ show activityBytes) ("event: activity" `BS.isInfixOf` activityBytes)
            assertBool "activity carries the command" ("grep -rln baseCommit /home" `BS.isInfixOf` activityBytes)
            pure pullAfter
    finishActiveTool logPath "t2"
    mCleared <- timeout 15000000 (pullUntilActivity activityPull)
    case mCleared of
        Nothing -> fail "activity clear did not arrive after the tool finished"
        Just clearedBytes -> do
            assertBool ("activity clear reports no call, got: " ++ show clearedBytes) ("\"call\":null" `BS.isInfixOf` clearedBytes)

pullUntilActivity :: IO (StepT IO BS.ByteString) -> IO BS.ByteString
pullUntilActivity step = do
    pulled <- pullStep step
    case pulled of
        Nothing -> fail "stream ended before an activity event"
        Just (bytes, rest)
            | "event: activity" `BS.isInfixOf` bytes -> pure bytes
            | otherwise -> pullUntilActivity rest
