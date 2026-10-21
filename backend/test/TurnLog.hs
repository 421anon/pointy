{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.TurnLog (appendLogLine, createTurnLog, readTurnLogFile)
import Agent.TurnSignal (registerTurnSignal)
import Config (defaultAgentConfig)
import Control.Concurrent.Async (forConcurrently_, race_)
import Control.Concurrent.STM (atomically, readTChan)
import Control.Monad (forM_, forever, unless)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "turn-log-test" $ \dir -> do
    let logPath = dir </> "turn.log"
    createTurnLog logPath ""
    appended <- registerTurnSignal logPath
    let liveStream = forever (atomically (readTChan appended) >> readTurnLogFile logPath)
    race_ liveStream $
        forConcurrently_ writers $ \label ->
            forM_ [1 .. linesPerWriter] $ \index ->
                appendLogLine defaultAgentConfig logPath label (numbered index)
    logged <- maybe [] T.lines <$> readTurnLogFile logPath
    forM_ writers $ \label -> do
        let written = filter (T.isPrefixOf (rendered label "")) logged
        assertBool
            ("every " ++ T.unpack label ++ " line is logged once and in order while a live stream re-reads the log, got " ++ show (length written) ++ " lines")
            (written == [rendered label (numbered index) | index <- [1 .. linesPerWriter]])

writers :: [Text]
writers = ["stdout", "stderr"]

linesPerWriter :: Int
linesPerWriter = 2000

numbered :: Int -> Text
numbered index = "line " <> T.pack (show index)

rendered :: Text -> Text -> Text
rendered label line = "[" <> label <> "] " <> line

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)
