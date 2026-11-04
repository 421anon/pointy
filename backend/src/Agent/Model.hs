{-# LANGUAGE OverloadedStrings #-}

module Agent.Model (AgentRole (..), agentRoleName, prepareChatModel, chatModelArgs) where

import Agent.PiConfig (writePiConfig)
import Config (ChatModelConfig (..), Config (..), agentChatModels, chatQualifier, loadConfig, resolveConfigPath)
import Data.Text (Text)
import qualified Data.Text as T

data AgentRole = Coder | Answerer
    deriving (Show, Eq)

agentRoleName :: AgentRole -> Text
agentRoleName role = case role of
    Coder -> "coder"
    Answerer -> "answerer"

agentChatModel :: AgentRole -> Config -> Maybe ChatModelConfig
agentChatModel role = case role of
    Coder -> configCoder
    Answerer -> configAnswerer

prepareChatModel :: AgentRole -> IO (Maybe ChatModelConfig)
prepareChatModel role = do
    config <- resolveConfigPath >>= loadConfig
    writePiConfig (configProviders config) (agentChatModels config)
    pure (agentChatModel role config)

chatModelArgs :: Maybe ChatModelConfig -> [String]
chatModelArgs Nothing = []
chatModelArgs (Just chat) = ["--model", T.unpack (chatQualifier chat)]
