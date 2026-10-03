{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Agent (
    TurnRequest (..),
    SessionRequest (..),
    RenameSessionRequest (..),
    createSessionHandler,
    getSessionHandler,
    listSessionsHandler,
    postTurnHandler,
    stopTurnHandler,
    steerTurnHandler,
    turnLogStreamHandler,
    archiveSessionHandler,
    renameSessionHandler,
    purgeSessionHandler,
    usageHandler,
) where

import Agent.Git (
    AgentSessionView,
    AgentUsage,
    archiveAgentSession,
    createAgentSession,
    getAgentUsage,
    listAgentSessions,
    loadAgentSessionView,
    purgeAgentSession,
    renameAgentSession,
 )
import Agent.Runner (startAgentTurn, steerAgentTurn, stopAgentTurn, turnLogStreamHandler)
import Agent.Session (AgentSessionSummary, AgentTurn)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Interpreters.Production (runProduction)
import Servant (Handler, NoContent (..), err400, err404, err409, err500, errBody, throwError)
import UserRepo (withUserRepoExclusiveIO, withUserRepoSharedIO)

data TurnRequest = TurnRequest
    { turnRequestSessionId :: Text
    , turnRequestPrompt :: Text
    , turnRequestCurrentProjectId :: Maybe Int
    }
    deriving (Show, Eq, Generic)

instance FromJSON TurnRequest where
    parseJSON = withObject "TurnRequest" $ \obj ->
        TurnRequest
            <$> obj .: "sessionId"
            <*> obj .: "prompt"
            <*> obj .:? "currentProjectId"

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

createSessionHandler :: Handler AgentSessionView
createSessionHandler = do
    sid <- runLockedAction createAgentSession
    runSharedAction (loadAgentSessionView sid)

listSessionsHandler :: Handler [AgentSessionSummary]
listSessionsHandler = runSharedAction listAgentSessions

getSessionHandler :: Text -> Handler AgentSessionView
getSessionHandler sid = runSharedAction (loadAgentSessionView sid)

postTurnHandler :: TurnRequest -> Handler AgentTurn
postTurnHandler req =
    runLockedAction $ startAgentTurn (turnRequestSessionId req) (turnRequestPrompt req) (turnRequestCurrentProjectId req)

stopTurnHandler :: SessionRequest -> Handler AgentSessionView
stopTurnHandler req =
    runAgentAction $ stopAgentTurn (sessionRequestSessionId req)

steerTurnHandler :: TurnRequest -> Handler NoContent
steerTurnHandler req =
    NoContent <$ runAgentAction (steerAgentTurn (turnRequestSessionId req) (turnRequestPrompt req))

renameSessionHandler :: RenameSessionRequest -> Handler AgentSessionView
renameSessionHandler req = do
    sid <- runLockedAction (renameAgentSession (renameSessionId req) (renameSessionName req))
    runSharedAction (loadAgentSessionView sid)

usageHandler :: Handler AgentUsage
usageHandler = liftIO $ withUserRepoSharedIO getAgentUsage

runLockedAction :: ExceptT String IO a -> Handler a
runLockedAction action = do
    result <- liftIO $ withUserRepoExclusiveIO action
    either throwAgentError return result

runSharedAction :: ExceptT String IO a -> Handler a
runSharedAction action = do
    result <- liftIO $ withUserRepoSharedIO (runExceptT action)
    either throwAgentError return result

runAgentAction :: ExceptT String IO a -> Handler a
runAgentAction action = do
    result <- liftIO $ runExceptT action
    either throwAgentError return result

throwAgentError :: String -> Handler a
throwAgentError err =
    throwError (fromMaybe err500 (lookup err statuses)){errBody = TLE.encodeUtf8 (TL.pack err)}
  where
    statuses =
        [ ("empty_session_name", err400)
        , ("empty_prompt", err400)
        , ("session_not_found", err404)
        ]
            ++ [(conflict, err409) | conflict <- conflicts]
    conflicts =
        [ "session_applied"
        , "session_discarded"
        , "session_archived"
        , "runner_active"
        , "runner_not_active"
        , "runner_stopping"
        , "steering_failed"
        ]

archiveSessionHandler :: SessionRequest -> Handler AgentSessionView
archiveSessionHandler req = do
    let sid = sessionRequestSessionId req
    _ <- runLockedAction (archiveAgentSession sid)
    runSharedAction (loadAgentSessionView sid)

purgeSessionHandler :: SessionRequest -> Handler NoContent
purgeSessionHandler req =
    NoContent <$ runLockedAction (purgeAgentSession (sessionRequestSessionId req))
