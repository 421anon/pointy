{-# LANGUAGE OverloadedStrings #-}

module Agent.Model (prepareChatModel, chatModelArgs) where

import Agent.PiConfig (writePiConfig)
import Config (ChatModelConfig (..), Config (..), chatQualifier, loadConfig, resolveConfigPath)
import qualified Data.Text as T

prepareChatModel :: IO (Maybe ChatModelConfig)
prepareChatModel = do
    config <- resolveConfigPath >>= loadConfig
    writePiConfig (configProviders config) (configChat config)
    pure (configChat config)

chatModelArgs :: Maybe ChatModelConfig -> [String]
chatModelArgs Nothing = []
chatModelArgs (Just chat) = ["--model", T.unpack (chatQualifier chat)]
