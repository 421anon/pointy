{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Search.ChatCorpus (
    chatCorpus,
) where

import Agent.Exchange (Exchange (..), exchangeReply, turnExchanges)
import Agent.Git (sessionDisplayTitle)
import Agent.Session (AgentSession (..), AgentTurn (..), isChangesetLifecycleTurn, listSessions, listTurns, listTurnsWithLogs)
import Config (EmbeddingConfig (..), JudgeConfig (..))
import Control.Exception (IOException, try)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as Base16
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (IOE, liftIO, (:>))
import Search.Store (Corpus (..), CorpusSnapshot (..), SearchDocument (..))

data ChatTranscript = ChatTranscript
    { transcriptSession :: AgentSession
    , transcriptTurns :: [AgentTurn]
    }

chatCorpus :: (IOE :> es) => Corpus es Text
chatCorpus =
    Corpus
        { corpusNoun = "chat"
        , corpusVectorDirectory = "chat-embeddings"
        , corpusQueryTemplate = embeddingChatQueryTemplate
        , corpusJudgeInstructions = judgeChatInstructions
        , corpusSnapshot = liftIO (either describeFailure (Right . snapshotOf) <$> try loadTranscripts)
        }
  where
    describeFailure :: IOException -> Either String a
    describeFailure err = Left (show err)
    snapshotOf transcripts =
        CorpusSnapshot
            { snapshotVersion = transcriptsVersion transcripts
            , snapshotDocuments = liftIO (either describeFailure Right <$> try (chatDocuments transcripts))
            }

loadTranscripts :: IO [ChatTranscript]
loadTranscripts = do
    sessions <- sortOn (Down . createdAt) <$> listSessions
    mapM (\session_ -> ChatTranscript session_ . sortOn turnStartedAt <$> listTurns (sessionId session_)) sessions

transcriptsVersion :: [ChatTranscript] -> Text
transcriptsVersion transcripts =
    hexDigest (hash (TE.encodeUtf8 (T.intercalate "\n" (map transcriptFingerprint transcripts))) :: Digest SHA256)
  where
    transcriptFingerprint transcript =
        T.intercalate
            "|"
            ( sessionId (transcriptSession transcript)
                : fromMaybe "" (sessionName (transcriptSession transcript))
                : [turnId turn <> ":" <> turnStatus turn | turn <- transcriptTurns transcript]
            )

chatDocuments :: [ChatTranscript] -> IO [SearchDocument Text]
chatDocuments transcripts = do
    texts <- mapM transcriptText transcripts
    pure
        [ SearchDocument
            { documentKey = sessionId (transcriptSession transcript)
            , documentJudgeId = position
            , documentText = text
            }
        | (position, transcript, Just text) <- zip3 [1 ..] transcripts texts
        ]

transcriptText :: ChatTranscript -> IO (Maybe Text)
transcriptText transcript = do
    turns <- filter conversational . sortOn turnStartedAt <$> listTurnsWithLogs (sessionId (transcriptSession transcript))
    let exchanges = concatMap turnExchanges turns
    pure $
        if null exchanges
            then Nothing
            else
                Just $
                    T.take maxDocumentLength $
                        T.intercalate "\n\n" (("Chat: " <> sessionDisplayTitle (transcriptSession transcript) (transcriptTurns transcript)) : map exchangeText exchanges)
  where
    conversational turn = not (turnAutomatic turn || isChangesetLifecycleTurn turn)
    exchangeText exchange =
        T.intercalate "\n" (filter (not . T.null) ["User: " <> T.strip (exchangeMessage exchange), assistantLine (exchangeReply exchange)])
    assistantLine reply
        | T.null reply = ""
        | otherwise = "Assistant: " <> reply

maxDocumentLength :: Int
maxDocumentLength = 6000

hexDigest :: Digest a -> Text
hexDigest = TE.decodeUtf8 . Base16.encode . convert
