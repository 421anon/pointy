{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Policy (renderAreaCapture)
import Agent.Session (AgentTurn (..), AreaCapture (..), StoredAreaCapture (..))
import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Exit (exitFailure)

main :: IO ()
main = do
    capture <- either fail pure (eitherDecode requestJson)
    let note = renderAreaCapture 3 4 (areaCaptureDocumentSize capture) (areaCaptureSelectionRect capture)
    check "the note names both attached images" (all (`T.isInfixOf` note) ["image 3:", "image 4:"])
    check "the note gives the document size" ("800 x 2400" `T.isInfixOf` note)
    check "the note gives the selection's position and size" ("x 40, y 300, width 400, height 200" `T.isInfixOf` note)
    startedAt <- iso8601ParseM "2026-01-01T00:00:00Z" :: IO UTCTime
    let turn = baseTurn startedAt
        stored = StoredAreaCapture 1 2 (areaCaptureDocumentSize capture) (areaCaptureSelectionRect capture)
    check "a turn keeps its area captures through JSON" (decode (encode turn{turnAreaCaptures = [stored]}) == Just turn{turnAreaCaptures = [stored]})
    check "a turn saved before area captures existed loads without any" (fmap turnAreaCaptures (decode turnWithoutAreaCapturesJson) == Just [])

baseTurn :: UTCTime -> AgentTurn
baseTurn startedAt =
    AgentTurn
        { turnId = "turn"
        , turnSessionId = "session"
        , turnPrompt = "prompt"
        , turnImages = ["image/png", "image/png", "image/png"]
        , turnAreaCaptures = []
        , turnAutomatic = False
        , turnStatus = "succeeded"
        , turnExitCode = Just 0
        , turnStartedAt = startedAt
        , turnFinishedAt = Nothing
        , turnLogPath = "turn.log"
        , turnLog = ""
        }

requestJson :: LBS.ByteString
requestJson = "{\"overview\":{\"mimeType\":\"image/png\",\"data\":\"aGk=\"},\"selection\":{\"mimeType\":\"image/png\",\"data\":\"aGk=\"},\"documentSize\":{\"width\":800,\"height\":2400},\"selectionRect\":{\"x\":40,\"y\":300,\"width\":400,\"height\":200}}"

turnWithoutAreaCapturesJson :: LBS.ByteString
turnWithoutAreaCapturesJson = "{\"turnId\":\"turn\",\"turnSessionId\":\"session\",\"turnStatus\":\"succeeded\",\"turnStartedAt\":\"2026-01-01T00:00:00Z\",\"turnLogPath\":\"turn.log\"}"

check :: String -> Bool -> IO ()
check label passed = if passed then pure () else putStrLn ("FAIL: " ++ label) >> exitFailure
