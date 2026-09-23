{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Session (
    AgentSession (..),
    AgentTurn (..),
    findTurn,
    forgetSessionTurns,
    listSessions,
    listTurns,
    saveSession,
    saveTurn,
    sessionDir,
 )
import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (listDirectory, removePathForcibly)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "session-store-test" $ \home -> do
    setEnv "HOME" home
    now <- getCurrentTime
    forM_ [1 .. 40] $ \n -> do
        let sid = sessionIdFor n
        saveSession (sessionFor home now sid)
        forM_ [1 .. 30] $ \t -> saveTurn (turnFor now sid t)
    before <- length <$> listDirectory "/proc/self/fd"
    sessions <- listSessions
    turnLists <- mapM (listTurns . sessionId) sessions
    found <- findTurn "turn-s40-30"
    after <- length <$> listDirectory "/proc/self/fd"
    assertBool "turn listing keeps file descriptors bounded" (after - before <= 16)
    assertEqual "all turns are listed" 1200 (sum (map length turnLists))
    assertEqual "findTurn resolves a turn in the last session" (Just "turn-s40-30") (turnId <$> found)

    let sid2 = "s-index"
    saveSession (sessionFor home now sid2)
    saveTurn (turnFor now sid2 1)
    _ <- listTurns sid2
    let finished = (turnFor now sid2 1){turnStatus = "succeeded", turnExitCode = Just 0, turnFinishedAt = Just now}
    saveTurn finished
    saveTurn (turnFor now sid2 2)
    indexed <- listTurns sid2
    assertEqual
        "indexed turns track status updates"
        [("turn-s-index-1", "succeeded"), ("turn-s-index-2", "running")]
        (sort [(turnId turn, turnStatus turn) | turn <- indexed])
    dir <- sessionDir sid2
    removePathForcibly dir
    forgetSessionTurns sid2
    remaining <- listTurns sid2
    assertEqual "forgotten sessions list no turns" [] remaining

    traversal <- findTurn "../../s1/turns/turn-s1-1"
    assertEqual "findTurn rejects path traversal ids" Nothing (turnId <$> traversal)
    missing <- findTurn "turn-missing"
    assertEqual "findTurn ignores unknown ids" Nothing (turnId <$> missing)

sessionIdFor :: Int -> Text
sessionIdFor n = "s" <> T.pack (show n)

sessionFor :: FilePath -> UTCTime -> Text -> AgentSession
sessionFor home now sid =
    AgentSession
        { sessionId = sid
        , sessionName = Nothing
        , targetBranch = "main"
        , agentBranch = "agent-branch"
        , baseCommit = "abc123"
        , worktreePath = home </> T.unpack sid
        , status = "open"
        , preparedApply = Nothing
        , activeTurnId = Nothing
        , lastError = Nothing
        , createdAt = now
        , updatedAt = now
        }

turnFor :: UTCTime -> Text -> Int -> AgentTurn
turnFor now sid n =
    AgentTurn
        { turnId = "turn-" <> sid <> "-" <> T.pack (show n)
        , turnSessionId = sid
        , turnPrompt = "prompt"
        , turnStatus = "running"
        , turnExitCode = Nothing
        , turnFinishedAt = Nothing
        , turnStartedAt = now
        , turnLogPath = ""
        , turnLog = ""
        }

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
