{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bus (broadcastSnapshot, subscribe)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Handlers.StatusStream (streamLoop)
import Handlers.Statuses (projectContainsStep)
import OutPaths (ProjectDef (..), decodeProjectDefinitions)
import Servant.Types.SourceT (StepT (..))
import System.Timeout (timeout)

main :: IO ()
main = do
    projects <- either fail pure (decodeProjectDefinitions projectsJson)
    assertEqual
        "step updates reach every project listing the step, hidden or not"
        [1, 2]
        [projectDefId p | p <- Map.elems projects, projectContainsStep 7 p]

    silent <- timeout 1000000 (pullStep (streamLoop =<< subscribe))
    assertEqual "no event without a broadcast" Nothing (fmap (const ()) silent)

    broadcastSnapshot 1 "abc123" (Map.singleton 1 ("success", Nothing))
    broadcastSnapshot 2 "def456" (Map.singleton 2 ("running", Nothing))

    m1 <- timeout 2000000 (pullStep (streamLoop =<< subscribe))
    case m1 of
        Nothing -> fail "first snapshot did not arrive"
        Just Nothing -> fail "stream ended before first snapshot"
        Just (Just (bytes1, pull1)) -> do
            assertBool "first snapshot is a snapshot event" ("event: snapshot" `BS.isInfixOf` bytes1)
            assertBool "first snapshot carries project id" ("\"projectId\":1" `BS.isInfixOf` bytes1)
            m2 <- timeout 2000000 (pullStep pull1)
            case m2 of
                Nothing -> fail "second snapshot did not arrive"
                Just Nothing -> fail "stream ended before second snapshot"
                Just (Just (bytes2, _pull2)) -> do
                    assertBool "second snapshot is a snapshot event" ("event: snapshot" `BS.isInfixOf` bytes2)
                    assertBool "second snapshot carries project id" ("\"projectId\":2" `BS.isInfixOf` bytes2)

projectsJson :: String
projectsJson =
    concat
        [ "{"
        , "\"1\":{\"id\":1,\"hidden\":false,\"steps\":[{\"hidden\":true,\"def\":{\"id\":7}}]},"
        , "\"2\":{\"id\":2,\"hidden\":true,\"steps\":[{\"hidden\":false,\"def\":{\"id\":7}}]},"
        , "\"3\":{\"id\":3,\"hidden\":false,\"steps\":[{\"hidden\":false,\"def\":{\"id\":8}}]}"
        , "}"
        ]

pullStep :: IO (StepT IO BS.ByteString) -> IO (Maybe (BS.ByteString, IO (StepT IO BS.ByteString)))
pullStep mstep = do
    step <- mstep
    case step of
        Yield bs rest -> pure (Just (bs, pure rest))
        Skip rest -> pullStep (pure rest)
        Effect m -> pullStep m
        Stop -> pure Nothing
        Error e -> fail ("stream error: " ++ show e)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
