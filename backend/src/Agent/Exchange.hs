{-# LANGUAGE OverloadedStrings #-}

module Agent.Exchange (Exchange (..), turnExchanges, exchangeReply) where

import Agent.Model (AgentRole (..), agentRoleName)
import Agent.Session (AgentTurn (..))
import Data.Aeson (Value, decodeStrict)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Exchange = Exchange
    { exchangeMessage :: Text
    , exchangeContext :: Maybe Value
    , exchangeRole :: Maybe AgentRole
    , exchangeReplyLines :: [Text]
    }

turnExchanges :: AgentTurn -> [Exchange]
turnExchanges turn = reverse (foldl' readLine [Exchange (turnPrompt turn) Nothing Nothing []] (T.lines (turnLog turn)))
  where
    readLine exchanges@(latest : earlier) line
        | Just name <- T.stripPrefix "[agent] " line =
            latest{exchangeRole = find ((== name) . agentRoleName) [Coder, Answerer]} : earlier
        | Just encoded <- T.stripPrefix "[judge-context] " line =
            latest{exchangeContext = decodeStrict (TE.encodeUtf8 encoded)} : earlier
        | Just encoded <- T.stripPrefix "[steering] " line
        , Just message <- decodeStrict (TE.encodeUtf8 encoded) =
            Exchange message Nothing (exchangeRole latest) [] : exchanges
        | Just text <- T.stripPrefix "[stdout] " line =
            latest{exchangeReplyLines = text : exchangeReplyLines latest} : earlier
        | otherwise = exchanges
    readLine [] _ = []

exchangeReply :: Exchange -> Text
exchangeReply = T.strip . T.unlines . reverse . exchangeReplyLines
