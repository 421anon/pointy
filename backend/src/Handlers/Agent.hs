{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Agent (
    TurnRequest (..),
    SessionRequest (..),
    RenameSessionRequest (..),
    ConfirmApplyRequest (..),
    createSessionHandler,
    getSessionHandler,
    listSessionsHandler,
    postTurnHandler,
    turnLogStreamHandler,
    prepareApplyHandler,
    confirmApplyHandler,
    discardSessionHandler,
    archiveSessionHandler,
    renameSessionHandler,
    purgeSessionHandler,
    usageHandler,
) where

import Agent.Git (
    AgentSessionView,
    AgentUsage,
    archiveAgentSession,
    confirmApplyCandidate,
    createAgentSession,
    discardAgentSession,
    getAgentUsage,
    listAgentSessions,
    loadAgentSessionView,
    prepareApplyCandidate,
    purgeAgentSession,
    renameAgentSession,
 )
import Agent.Runner (startAgentTurn, turnLogStreamHandler)
import Agent.Session (AgentTurn)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Servant (Handler, NoContent (..), err400, err409, err500, errBody, throwError)
import UserRepo (withUserRepoExclusive)

data TurnRequest = TurnRequest
    { turnRequestSessionId :: Text
    , turnRequestPrompt :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON TurnRequest where
    parseJSON = withObject "TurnRequest" $ \obj ->
        TurnRequest
            <$> obj .: "sessionId"
            <*> obj .: "prompt"

data SessionRequest = SessionRequest
    { sessionRequestSessionId :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON SessionRequest where
    parseJSON = withObject "SessionRequest" $ \obj ->
        SessionRequest <$> obj .: "sessionId"

data RenameSessionRequest = RenameSessionRequest
    { renameSessionId :: Text
    , renameSessionName :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON RenameSessionRequest where
    parseJSON = withObject "RenameSessionRequest" $ \obj ->
        RenameSessionRequest
            <$> obj .: "sessionId"
            <*> obj .: "name"

data ConfirmApplyRequest = ConfirmApplyRequest
    { confirmSessionId :: Text
    , confirmTargetHead :: Text
    , confirmCandidateHead :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON ConfirmApplyRequest where
    parseJSON = withObject "ConfirmApplyRequest" $ \obj ->
        ConfirmApplyRequest
            <$> obj .: "sessionId"
            <*> obj .: "targetHead"
            <*> obj .: "candidateHead"

createSessionHandler :: Handler AgentSessionView
createSessionHandler = runLockedAction createAgentSession

listSessionsHandler :: Handler [AgentSessionView]
listSessionsHandler = runLockedAction listAgentSessions

getSessionHandler :: Text -> Handler AgentSessionView
getSessionHandler sid = runAgentAction $ loadAgentSessionView sid

postTurnHandler :: TurnRequest -> Handler AgentTurn
postTurnHandler req =
    runLockedAction $ startAgentTurn (turnRequestSessionId req) (turnRequestPrompt req)

prepareApplyHandler :: SessionRequest -> Handler AgentSessionView
prepareApplyHandler req =
    runLockedAction $ prepareApplyCandidate (sessionRequestSessionId req)

confirmApplyHandler :: ConfirmApplyRequest -> Handler AgentSessionView
confirmApplyHandler req =
    runLockedAction $ confirmApplyCandidate (confirmSessionId req) (confirmTargetHead req) (confirmCandidateHead req)

discardSessionHandler :: SessionRequest -> Handler AgentSessionView
discardSessionHandler req =
    runLockedAction $ discardAgentSession (sessionRequestSessionId req)

renameSessionHandler :: RenameSessionRequest -> Handler AgentSessionView
renameSessionHandler req =
    runLockedAction $ renameAgentSession (renameSessionId req) (renameSessionName req)

usageHandler :: Handler AgentUsage
usageHandler = liftIO getAgentUsage

runLockedAction :: ExceptT String IO a -> Handler a
runLockedAction action = do
    result <- liftIO $ withUserRepoExclusive action
    either throwAgentError return result

runAgentAction :: ExceptT String IO a -> Handler a
runAgentAction action = do
    result <- liftIO $ runExceptT action
    either throwAgentError return result

throwAgentError :: String -> Handler a
throwAgentError err =
    let baseError =
            case err of
                "empty_session_name" ->
                    err400
                _ ->
                    if err `elem` ["session_applied", "session_discarded", "session_archived", "runner_active"]
                        then
                            err409
                        else
                            err500
     in throwError baseError{errBody = TLE.encodeUtf8 (TL.pack err)}

archiveSessionHandler :: SessionRequest -> Handler AgentSessionView
archiveSessionHandler req =
    runLockedAction $ archiveAgentSession (sessionRequestSessionId req)

purgeSessionHandler :: SessionRequest -> Handler NoContent
purgeSessionHandler req = do
    _ <- runLockedAction $ purgeAgentSession (sessionRequestSessionId req)
    return NoContent
