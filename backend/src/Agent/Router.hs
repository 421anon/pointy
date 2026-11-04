{-# LANGUAGE OverloadedStrings #-}

module Agent.Router (Route (..), routeTurn) where

import Agent.Model (AgentRole (..), agentRoleName)
import Agent.Policy (RoutingChoice (..), routingChoices, routingInstructions)
import Agent.Session (AgentTurn (..), isChangesetLifecycleTurn, listTurnsWithLogs)
import Config (Config (..), RouterConfig (..), loadConfig, resolveConfigPath)
import Data.Aeson (Value, decodeStrict, object, (.=))
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import ProviderHttp (choiceRequest)

data Route
    = SoleCoder
    | Routed AgentRole (Maybe Text)

data Exchange = Exchange
    { exchangeMessage :: Text
    , exchangeRole :: Maybe AgentRole
    , exchangeReplyLines :: [Text]
    }

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

turnExchanges :: AgentTurn -> [Exchange]
turnExchanges turn = reverse (foldl' readLine [Exchange (turnPrompt turn) Nothing []] (T.lines (turnLog turn)))
  where
    readLine exchanges@(latest : earlier) line
        | Just name <- T.stripPrefix "[agent] " line =
            latest{exchangeRole = find ((== name) . agentRoleName) [Coder, Answerer]} : earlier
        | Just encoded <- T.stripPrefix "[steering] " line
        , Just message <- decodeStrict (TE.encodeUtf8 encoded) =
            Exchange message (exchangeRole latest) [] : exchanges
        | Just text <- T.stripPrefix "[stdout] " line =
            latest{exchangeReplyLines = text : exchangeReplyLines latest} : earlier
        | otherwise = exchanges
    readLine [] _ = []

exchangeJson :: Exchange -> Value
exchangeJson exchange =
    object $
        [ "message" .= exchangeMessage exchange
        , "reply" .= T.strip (T.unlines (reverse (exchangeReplyLines exchange)))
        ]
            ++ [ "routedTo" .= choiceLabel choice
               | Just role <- [exchangeRole exchange]
               , Just choice <- [find ((== role) . choiceRole) routingChoices]
               ]
