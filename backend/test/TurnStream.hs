{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the event-driven turn log stream: streams must wake on
signals (log appends, turn state saves) instead of polling the log
file, and must emit @chunk@/@done@ in the same shape as before.
-}
module Main (main) where

import Agent.Runner (streamLoop)
import Agent.Session (AgentSession (..), AgentTurn (..), saveSession, saveTurn, turnLogFilePath)
import Agent.TurnSignal (registerTurnSignal, signalTurnLog)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Servant.Types.SourceT (StepT (..))
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

main :: IO ()
main = withSystemTempDirectory "turn-stream-test" $ \home -> do
    -- Point the session store at the temp dir.
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

    -- Silent while nothing happens: no polling, no periodic comments.
    nothing1 <- timeout 1000000 (pullStep pull0)
    assertEqual "no event without any signal" Nothing (fmap (const ()) nothing1)

    -- A log append alone must not wake the stream either.
    TIO.appendFile logPath "[stdout] first line\n"
    nothing2 <- timeout 1000000 (pullStep pull0)
    assertEqual "no event after append without signal" Nothing (fmap (const ()) nothing2)

    -- The signal delivers the append as a chunk event.
    signalTurnLog logPath
    mChunk <- timeout 2000000 (pullStep pull0)
    case mChunk of
        Nothing -> fail "chunk event did not arrive after signal"
        Just Nothing -> fail "stream ended before chunk"
        Just (Just (chunkBytes, pull1)) -> do
            assertBool "chunk event name" ("event: chunk" `BS.isInfixOf` chunkBytes)
            assertBool "chunk carries log line" ("first line" `BS.isInfixOf` chunkBytes)
            -- Saving the finished turn wakes the stream (saveTurn signals);
            -- it must then emit done and close.
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
                        -- Clean end: the stream stopped after done.
                        Just Nothing -> pure ()
                        Nothing -> fail "stream stayed open after done"
                        Just _ -> fail "stream emitted an event after done"

-- | Pull one SSE event (or the stream end) from a step producer.
pullStep :: IO (StepT IO BS.ByteString) -> IO (Maybe (BS.ByteString, IO (StepT IO BS.ByteString)))
pullStep mstep = do
    step <- mstep
    case step of
        Yield bs rest -> pure (Just (bs, pure rest))
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
