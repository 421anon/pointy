{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bus (broadcastSnapshot, subscribe)
import Control.Monad (unless)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Handlers.StatusStream (streamLoop)
import Handlers.Statuses (projectContainsStep)
import Certificates (ProjectDef (..), decodeProjectDefinitions)
import Servant.Types.SourceT (StepT (..))
import System.Timeout (timeout)

main :: IO ()
main = do
    projects <- either fail pure (eitherDecode projectsJson >>= decodeProjectDefinitions)
    assertEqual
        "step updates reach every project listing the step, hidden or not, and no project listing a project with the step's id"
        [1, 2]
        [projectDefId p | p <- Map.elems projects, projectContainsStep 7 p]

    silent <- timeout 1000000 (pullStep (streamLoop =<< subscribe))
    assertEqual "no event without a broadcast" Nothing (fmap (const ()) silent)

    broadcastSnapshot 1 "abc123" (Map.singleton 1 "/nix/store/abc-certificate") (Map.singleton 1 ("success", Nothing))
    broadcastSnapshot 2 "def456" Map.empty (Map.singleton 2 ("running", Nothing))

    m1 <- timeout 2000000 (pullStep (streamLoop =<< subscribe))
    case m1 of
        Nothing -> fail "first snapshot did not arrive"
        Just Nothing -> fail "stream ended before first snapshot"
        Just (Just (bytes1, pull1)) -> do
            assertBool "first snapshot is a snapshot event" ("event: snapshot" `BS.isInfixOf` bytes1)
            assertBool "first snapshot carries project id" ("\"projectId\":1" `BS.isInfixOf` bytes1)
            assertBool "first snapshot carries the step certificate clients compare across commits" ("\"certificate\":\"/nix/store/abc-certificate\"" `BS.isInfixOf` bytes1)
            m2 <- timeout 2000000 (pullStep pull1)
            case m2 of
                Nothing -> fail "second snapshot did not arrive"
                Just Nothing -> fail "stream ended before second snapshot"
                Just (Just (bytes2, _pull2)) -> do
                    assertBool "second snapshot is a snapshot event" ("event: snapshot" `BS.isInfixOf` bytes2)
                    assertBool "second snapshot carries project id" ("\"projectId\":2" `BS.isInfixOf` bytes2)

projectsJson :: LBS.ByteString
projectsJson =
    mconcat
        [ "{"
        , "\"1\":{\"id\":1,\"children\":[{\"step\":{\"id\":7,\"hidden\":true,\"sortKey\":null,\"def\":{\"id\":7}}}]},"
        , "\"2\":{\"id\":2,\"children\":[{\"project\":{\"id\":1,\"hidden\":false,\"sortKey\":null}},{\"step\":{\"id\":7,\"hidden\":false,\"sortKey\":null,\"def\":{\"id\":7}}}]},"
        , "\"3\":{\"id\":3,\"children\":[{\"step\":{\"id\":8,\"hidden\":false,\"sortKey\":null,\"def\":{\"id\":8}}},{\"project\":{\"id\":7,\"hidden\":false,\"sortKey\":null}}]}"
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
