{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Exchange (Exchange (..), exchangeReply)
import Agent.Router (capExchangeHistory, routerHistoryCharacterBudget)
import Control.Monad (unless)
import qualified Data.Text as T

main :: IO ()
main = do
    keepsRecentExchangesWithinBudget
    truncatesLongReplies

keepsRecentExchangesWithinBudget :: IO ()
keepsRecentExchangesWithinBudget = do
    let exchanges = [exchange index | index <- [0 .. 59]]
        capped = capExchangeHistory exchanges
        total = sum (map characterCount capped)
    assertBool "capped history stays within the character budget" (total <= routerHistoryCharacterBudget)
    assertBool "capped history keeps only recent exchanges" (length capped < length exchanges)
    assertEqual "capped history keeps the latest exchange" (exchangeMessage (last exchanges)) (exchangeMessage (last capped))
    assertBool "capped history drops the oldest exchange" $
        case exchanges of
            oldest : _ -> all ((/= exchangeMessage oldest) . exchangeMessage) capped
            [] -> False

truncatesLongReplies :: IO ()
truncatesLongReplies = do
    let longReply = T.replicate 50000 "x"
        capped = capExchangeHistory [Exchange "old question" Nothing Nothing ["short reply"], Exchange "latest question" Nothing Nothing [longReply]]
        latestReply = exchangeReply (last capped)
    assertBool "a long reply is truncated" (T.length latestReply < T.length longReply)
    assertBool "a truncated reply carries a marker" ("[truncated]" `T.isInfixOf` latestReply)
    assertEqual "truncation keeps the latest exchange" "latest question" (exchangeMessage (last capped))

exchange :: Int -> Exchange
exchange index =
    Exchange
        { exchangeMessage = "message " <> T.pack (show index)
        , exchangeContext = Nothing
        , exchangeRole = Nothing
        , exchangeReplyLines = [T.replicate 1000 "r"]
        }

characterCount :: Exchange -> Int
characterCount exchange_ =
    T.length (exchangeMessage exchange_) + T.length (exchangeReply exchange_)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
