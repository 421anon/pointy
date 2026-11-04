{-# LANGUAGE OverloadedStrings #-}

module Agent.Router (Route (..), routeTurn) where

import Agent.Model (AgentRole (..), agentRoleName)
import Agent.Policy (routingCriteria, routingInstructions)
import Config (Config (..), RouterConfig (..), loadConfig, resolveConfigPath)
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import ProviderHttp (choiceRequest)

data Route
    = SoleCoder
    | Routed AgentRole (Maybe Text)

routeTurn :: Maybe Text -> IO Route
routeTurn mRequest = do
    config <- resolveConfigPath >>= loadConfig
    case (configRouter config, mRequest) of
        (Nothing, _) -> pure SoleCoder
        (Just _, Nothing) -> pure (Routed Coder Nothing)
        (Just router, Just request) -> case Map.lookup (routerProvider router) (configProviders config) of
            Nothing -> pure (failed ("the provider \"" <> T.unpack (routerProvider router) <> "\" is not configured"))
            Just provider -> either failed routed <$> choiceRequest provider (routerModel router) (object ["message" .= request]) routingInstructions criteria
  where
    criteria = [(agentRoleName role, meaning) | (role, meaning) <- routingCriteria]
    routed label = case lookup label [(agentRoleName role, role) | (role, _) <- routingCriteria] of
        Just role -> Routed role Nothing
        Nothing -> failed ("the router chose \"" <> T.unpack label <> "\"")
    failed reason = Routed Coder (Just ("Routing failed, so the coder takes this turn: " <> T.pack reason))
