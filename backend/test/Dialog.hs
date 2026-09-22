{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Runner (RunnerInput, SteerOutcome (..), handleDialog, handleRpcEventSafely, newRunnerInput, planSteer, streamHandle)
import Config (defaultAgentConfig)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (newEmptyTMVarIO)
import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
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
    checkMultiSelectDialog dir
    checkCustomAnswerDialog dir
    checkReaderSurvivesFailure dir
    checkDeclinedDialogKeepsOtherQuestion dir

checkSelectDialog :: FilePath -> IO ()
checkSelectDialog dir = withInput $ \input -> do
    let logPath = dir </> "select.log"
    handleDialog defaultAgentConfig logPath input (selectEvent "d1")
    logged <- TIO.readFile logPath
    assertContains "select: the question reaches the chat" "[stdout] Which layout should the demo use?" logged
    assertEqual "select: only the pickable rows become buttons" (Just (False, ["1. Compact card - a small card.", "2. Wide table row - one line."])) (questionIn logged)
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "2" "steer-1" reply control of
        Just (response, _, SteerAnsweredQuestion answer) -> do
            assertEqual "select: the picked number reaches the dialog" (dialogValue "d1" "2") response
            assertEqual "select: the chat names the picked row" "2. Wide table row - one line." answer
        Just _ -> fail "select: a picked answer should answer the question rather than steer the agent"
        Nothing -> fail "select: a picked answer should answer the question, not steer the agent"
    typed <- latchedQuestion input
    reply2 <- newEmptyTMVarIO
    case planSteer "something else" "steer-2" reply2 typed of
        Just (response, _, SteerAnsweredQuestion _) -> assertEqual "select: a typed answer is sent as the dialog's free-text row" (dialogValue "d1" "3") response
        Just _ -> fail "select: a typed answer should answer the question rather than steer the agent"
        Nothing -> fail "select: a typed answer should answer the question, not steer the agent"

checkMultiSelectDialog :: FilePath -> IO ()
checkMultiSelectDialog dir = do
    withInput $ \input -> do
        let logPath = dir </> "multi.log"
        handleDialog defaultAgentConfig logPath input multiSelectDialog
        logged <- TIO.readFile logPath
        assertContains "multi: the question reaches the chat" "[stdout] [Outputs] Which outputs" logged
        assertEqual "multi: the numbered rows in the title become a pick list" (Just (True, multiRows)) (questionIn logged)
        control <- latchedQuestion input
        reply <- newEmptyTMVarIO
        case planSteer "1,3" "steer-3" reply control of
            Just (response, _, SteerAnsweredQuestion answer) -> do
                assertEqual "multi: the picked numbers reach the dialog whole" (dialogValue "d2" "1,3") response
                assertEqual "multi: the chat names the rows those numbers picked" (T.intercalate ", " [multiRows !! 0, multiRows !! 2]) answer
            Just _ -> fail "multi: a picked answer should answer the question rather than steer the agent"
            Nothing -> fail "multi: a picked answer should answer the question, not steer the agent"
    withInput $ \input -> do
        let logPath = dir </> "multi-typed.log"
        handleDialog defaultAgentConfig logPath input multiSelectDialog
        typed <- latchedQuestion input
        reply <- newEmptyTMVarIO
        case planSteer "please use fastp" "steer-4" reply typed of
            Just (response, _, SteerAnsweredQuestion answer) -> do
                assertEqual "multi: a typed answer reaches the dialog verbatim" (dialogValue "d2" "please use fastp") response
                assertEqual "multi: a typed answer stays as it was typed" "please use fastp" answer
            Just _ -> fail "multi: a typed answer should answer the question rather than steer the agent"
            Nothing -> fail "multi: a typed answer should answer the question, not steer the agent"

checkCustomAnswerDialog :: FilePath -> IO ()
checkCustomAnswerDialog dir = withInput $ \input -> do
    let logPath = dir </> "custom.log"
    handleDialog defaultAgentConfig logPath input customAnswerDialog
    logged <- TIO.readFile logPath
    assertContains "custom: the question reaches the chat" "[stdout] Which layout should the demo use?" logged
    assertEqual "custom: a question without a numbered block offers no buttons" Nothing (questionIn logged)
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "2" "steer-5" reply control of
        Just (response, _, SteerAnsweredQuestion _) -> assertEqual "custom: the typed answer reaches the dialog verbatim" (dialogValue "d3" "2") response
        Just _ -> fail "custom: the typed answer should answer the question rather than steer the agent"
        Nothing -> fail "custom: the typed answer should answer the question, not steer the agent"

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
    case planSteer "hello" "steer-6" reply control of
        Just (response, _, _) -> assertEqual "reader: the declined dialog drops its question" (steer "steer-6" "hello") response
        Nothing -> fail "reader: the message should steer the agent, not park on the dialog"

checkDeclinedDialogKeepsOtherQuestion :: FilePath -> IO ()
checkDeclinedDialogKeepsOtherQuestion dir = withInput $ \input -> do
    let logPath = dir </> "other.log"
    handleDialog defaultAgentConfig logPath input (selectEvent "d1")
    handleRpcEventSafely defaultAgentConfig logPath input (selectEvent "d2")
    control <- latchedQuestion input
    reply <- newEmptyTMVarIO
    case planSteer "1" "steer-7" reply control of
        Just (response, _, SteerAnsweredQuestion _) -> assertEqual "other: the open question still answers the steer" (dialogValue "d1" "1") response
        Just _ -> fail "other: the open question should answer the steer rather than pass it on"
        Nothing -> fail "other: the open question should answer the steer"

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

multiSelectDialog :: Value
multiSelectDialog =
    object
        [ "type" .= ("extension_ui_request" :: Text)
        , "id" .= ("d2" :: Text)
        , "method" .= ("input" :: Text)
        , "title" .= multiTitle
        , "placeholder" .= ("1,3" :: Text)
        ]

multiTitle :: Text
multiTitle =
    "[Outputs] Which outputs should the step publish to its output folder?\n\n"
        <> T.intercalate "\n" multiRows
        <> "\n\nEnter the numbers of all that apply, comma-separated (e.g. \"1,3\"), or type a custom answer as plain text."

multiRows :: [Text]
multiRows =
    [ "1. Cleaned FASTQ files — Publish the cleaned per-sample read files."
    , "2. Quality report — Publish the fastp HTML/JSON quality report."
    , "3. Read count table — Publish a table of read counts before and after cleaning."
    , "4. Run log — Publish the full text log of the cleaning run."
    ]

customAnswerDialog :: Value
customAnswerDialog =
    object
        [ "type" .= ("extension_ui_request" :: Text)
        , "id" .= ("d3" :: Text)
        , "method" .= ("input" :: Text)
        , "title" .= ("Which layout should the demo use?\n\nType your answer:" :: Text)
        , "placeholder" .= ("" :: Text)
        ]

dialogValue :: Text -> Text -> Value
dialogValue dialogId value =
    object ["type" .= ("extension_ui_response" :: Text), "id" .= dialogId, "value" .= value]

steer :: Text -> Text -> Value
steer requestId prompt =
    object ["id" .= requestId, "type" .= ("prompt" :: Text), "message" .= prompt, "streamingBehavior" .= ("steer" :: Text)]

questionIn :: Text -> Maybe (Bool, [Text])
questionIn = listToMaybe . mapMaybe question . T.lines
  where
    question line = do
        body <- T.stripPrefix "[question] " line
        value <- Aeson.decodeStrict (TE.encodeUtf8 body)
        parseMaybe (Aeson.withObject "question" (\o -> (,) <$> o Aeson..: "multi" <*> o Aeson..: "options")) value

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
