{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the RPC dialog bridge: pi's @extension_ui_request@ events become
chat questions, the package's options-less dialog (its multi-select question)
offers no buttons, and no handler failure may take the reader down with it.
-}
module Main (main) where

import Agent.Runner (RunnerInput, handleDialog, handleRpcEventSafely, newRunnerInput, planSteer, streamHandle)
import Config (defaultAgentConfig)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Control.Monad (unless, (>=>))
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hSetBuffering)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Timeout (timeout)

main :: IO ()
main = withSystemTempDirectory "dialog-test" $ \dir -> do
    checkSelectDialog dir
    checkOptionsLessDialog dir
    checkReaderSurvivesFailure dir
    checkDeclinedDialogKeepsOtherQuestion dir

checkSelectDialog :: FilePath -> IO ()
checkSelectDialog dir = withInput $ \input -> do
    let logPath = dir </> "select.log"
    handleDialog defaultAgentConfig logPath input (selectEvent "d1")
    logged <- TIO.readFile logPath
    assertContains "select: the question reaches the chat" "[stdout] Which layout should the demo use?" logged
    assertEqual "select: only the pickable rows become buttons" (Just ["1. Compact card - a small card.", "2. Wide table row - one line."]) (questionRowsIn logged)

checkOptionsLessDialog :: FilePath -> IO ()
checkOptionsLessDialog dir = withInput $ \input -> do
    let logPath = dir </> "input.log"
    handleDialog defaultAgentConfig logPath input optionsLessDialog
    logged <- TIO.readFile logPath
    assertContains "input: the question reaches the chat" "[stdout] [Features] Which optional features" logged
    assertEqual "input: an options-less dialog offers no buttons" Nothing (questionRowsIn logged)
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "1,3" "steer-1" reply control of
        Just (response, _, _) -> assertEqual "input: the typed answer is sent to pi as the dialog's value" (dialogValue "d2" "1,3") response
        Nothing -> fail "input: the typed answer should answer the question, not steer the agent"

checkReaderSurvivesFailure :: FilePath -> IO ()
checkReaderSurvivesFailure dir = withInput $ \input -> do
    (readFd, writeFd) <- createPipe
    readerHandle <- fdToHandle readFd
    writer <- fdToHandle writeFd
    hSetBuffering writer LineBuffering
    let logPath = dir </> "reader.log"
        cfg = defaultAgentConfig
    reader <- async $ streamHandle cfg logPath "__BEGIN__" "stdout" (handleRpcEventSafely cfg logPath input) readerHandle
    TIO.hPutStrLn writer "__BEGIN__"
    -- The same dialog twice: its reply cannot be written, and the next event is
    -- still read.
    writeLine writer (selectEvent "d1")
    writeLine writer (selectEvent "d1")
    writeLine writer (object ["type" .= ("compaction_start" :: Text)])
    hClose writer
    finished <- timeout 5000000 (wait reader)
    assertBool "reader: the loop ends when pi's stream does" (isJust finished)
    logged <- TIO.readFile logPath
    assertContains "reader: the handler failure is noted" "[system] Could not handle the runner's extension_ui_request event" logged
    assertContains "reader: the loop keeps reading after a handler failure" "[stdout] *Summarising the conversation so far*" logged
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "hello" "steer-2" reply control of
        Just (response, _, _) -> assertEqual "reader: the declined dialog drops its question" (steer "steer-2" "hello") response
        Nothing -> fail "reader: the message should steer the agent, not park on the dialog"

checkDeclinedDialogKeepsOtherQuestion :: FilePath -> IO ()
checkDeclinedDialogKeepsOtherQuestion dir = withInput $ \input -> do
    let logPath = dir </> "other.log"
    handleDialog defaultAgentConfig logPath input (selectEvent "d1")
    handleRpcEventSafely defaultAgentConfig logPath input (selectEvent "d2")
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "1" "steer-3" reply control of
        Just (response, _, _) -> assertEqual "other: the open question still answers the steer" (dialogValue "d1" "1") response
        Nothing -> fail "other: the open question should answer the steer"

-- | A runner input whose pipe has no reader: every write to pi fails.
withInput :: (MVar (Maybe RunnerInput) -> IO a) -> IO a
withInput action = do
    (readFd, writeFd) <- createPipe
    closeFd readFd
    input <- newRunnerInput =<< fdToHandle writeFd
    action input

latchedQuestion :: MVar (Maybe RunnerInput) -> IO RunnerInput
latchedQuestion input = do
    latched <- readMVar input
    maybe (fail "the dialog should leave a question latched") return latched

selectEvent :: Text -> Value
selectEvent dialogId =
    object
        [ "type" .= ("extension_ui_request" :: Text)
        , "id" .= dialogId
        , "method" .= ("select" :: Text)
        , "title" .= ("Which layout should the demo use?" :: Text)
        , "options" .= (["1. Compact card - a small card.", "2. Wide table row - one line.", "3. Type something."] :: [Text])
        ]

optionsLessDialog :: Value
optionsLessDialog =
    object
        [ "type" .= ("extension_ui_request" :: Text)
        , "id" .= ("d2" :: Text)
        , "method" .= ("input" :: Text)
        , "title" .= ("[Features] Which optional features should the demo enable? (Select all that apply.)\n\n1. Previews\n2. Multi-select\n\nEnter the numbers of all that apply." :: Text)
        , "placeholder" .= ("1,3" :: Text)
        ]

dialogValue :: Text -> Text -> Value
dialogValue dialogId value =
    object ["type" .= ("extension_ui_response" :: Text), "id" .= dialogId, "value" .= value]

steer :: Text -> Text -> Value
steer requestId prompt =
    object ["id" .= requestId, "type" .= ("prompt" :: Text), "message" .= prompt, "streamingBehavior" .= ("steer" :: Text)]

questionRowsIn :: Text -> Maybe [Text]
questionRowsIn = listToMaybe . mapMaybe (T.stripPrefix "[question] ") . T.lines >=> Aeson.decodeStrict . TE.encodeUtf8

writeLine :: Handle -> Value -> IO ()
writeLine handle = TIO.hPutStrLn handle . TE.decodeUtf8 . LBS.toStrict . Aeson.encode

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertContains :: String -> Text -> Text -> IO ()
assertContains label needle logged =
    unless (needle `T.isInfixOf` logged) $
        fail (label ++ ": missing " ++ show needle ++ " in the turn log:\n" ++ T.unpack logged)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
