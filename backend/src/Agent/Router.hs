{-# LANGUAGE OverloadedStrings #-}

module Agent.Router (Route (..), routeTurn) where

import Agent.Exchange (Exchange (..), exchangeReply, turnExchanges)
import Agent.Model (AgentRole (..))
import Agent.Policy (RoutingChoice (..), routingChoices, routingInstructions)
import Agent.Session (AgentTurn (..), isChangesetLifecycleTurn, listTurnsWithLogs)
import Config (Config (..), RouterConfig (..), loadConfig, resolveConfigPath)
import Data.Aeson (Value, object, (.=))
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import ProviderHttp (choiceRequest)

data Route
    = SoleCoder
    | Routed AgentRole (Maybe Text)

routeTurn :: AgentTurn -> Maybe Text -> IO Route
routeTurn turn mRequest = do
    config <- resolveConfigPath >>= loadConfig
    case (configRouter config, mRequest) of
        (Nothing, _) -> pure SoleCoder
        (Just _, Nothing) -> pure (Routed Coder Nothing)
        (Just router, Just request) -> case Map.lookup (routerProvider router) (configProviders config) of
            Nothing -> pure (failed ("the provider \"" <> T.unpack (routerProvider router) <> "\" is not configured"))
            Just provider -> do
                earlier <- earlierExchanges turn
                let conversation = map exchangeJson earlier ++ [object ["message" .= request]]
                either failed routed <$> choiceRequest provider (routerModel router) (object ["conversation" .= conversation]) routingInstructions criteria
  where
    criteria = [(choiceLabel choice, choiceMeaning choice) | choice <- routingChoices]
    routed label = case find ((== label) . choiceLabel) routingChoices of
        Just choice -> Routed (choiceRole choice) Nothing
        Nothing -> failed ("the router chose \"" <> T.unpack label <> "\"")
    failed reason = Routed Coder (Just ("Routing failed, so the coder takes this turn: " <> T.pack reason))

earlierExchanges :: AgentTurn -> IO [Exchange]
earlierExchanges current = do
    turns <- listTurnsWithLogs (turnSessionId current)
    pure
        [ exchange
        | turn <- sortOn turnStartedAt turns
        , turnId turn /= turnId current
        , turnStartedAt turn <= turnStartedAt current
        , not (isChangesetLifecycleTurn turn)
        , exchange <- turnExchanges turn
        ]

exchangeJson :: Exchange -> Value
exchangeJson exchange =
    object $
        [ "message" .= exchangeMessage exchange
        , "reply" .= exchangeReply exchange
        ]
            ++ [ "routedTo" .= choiceLabel choice
               | Just role <- [exchangeRole exchange]
               , Just choice <- [find ((== role) . choiceRole) routingChoices]
               ]
