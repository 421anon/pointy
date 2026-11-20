{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bus (broadcastSnapshot)
import Certificates (ProjectDef (..), decodeProjectDefinitions)
import Control.Monad (unless, void)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Handlers.Events (eventsStream)
import Handlers.Statuses (projectContainsStep)
import IngestBus (reserveJob)
import Servant.Types.SourceT (StepT (..))
import System.Timeout (timeout)

main :: IO ()
main = do
    projects <- either fail pure (eitherDecode projectsJson >>= decodeProjectDefinitions)
    assertEqual
        "step updates reach every project listing the step, hidden or not, and no project listing a project with the step's id"
        [1, 2]
        [projectDefId p | p <- Map.elems projects, projectContainsStep 7 p]

    early <- pure <$> eventsStream
    (clusterOnConnect, afterCluster) <- nextEvent "cluster status on connect" early
    assertBool "a connection opens with the cluster status" ("event: cluster-status" `BS.isInfixOf` clusterOnConnect)
    (ingestOnConnect, afterIngest) <- nextEvent "ingest jobs on connect" afterCluster
    assertBool "a connection then sends the ingest jobs" ("event: ingest-jobs" `BS.isInfixOf` ingestOnConnect)
    silent <- timeout 1000000 (pullStep afterIngest)
    assertEqual "no event without a change" Nothing (void silent)

    void (reserveJob 7 "scratch" (Just "reads"))
    (ingestUpdate, afterIngestUpdate) <- nextEvent "ingest job update" afterIngest
    assertBool "a started ingest job reaches an open connection" ("event: ingest-jobs" `BS.isInfixOf` ingestUpdate && "\"stepId\":7" `BS.isInfixOf` ingestUpdate)

    broadcastSnapshot 1 "abc123" (Map.singleton 1 "/nix/store/abc-certificate") (Map.singleton 1 ("success", Nothing))
    (statusUpdate, _) <- nextEvent "step status update" afterIngestUpdate
    assertBool "a step status reaches the same connection" ("event: step-status" `BS.isInfixOf` statusUpdate && "\"projectId\":1" `BS.isInfixOf` statusUpdate)
    assertBool "a step status carries the certificate clients compare across commits" ("\"certificate\":\"/nix/store/abc-certificate\"" `BS.isInfixOf` statusUpdate)

    broadcastSnapshot 2 "def456" Map.empty (Map.singleton 2 ("running", Nothing))
    late <- pure <$> eventsStream
    (_, lateAfterCluster) <- nextEvent "cluster status on a later connect" late
    (lateIngest, lateAfterIngest) <- nextEvent "ingest jobs on a later connect" lateAfterCluster
    assertBool "a later connection receives the running ingest job" ("\"stepId\":7" `BS.isInfixOf` lateIngest)
    (firstReplay, afterFirstReplay) <- nextEvent "first replayed step status" lateAfterIngest
    assertBool "a later connection replays the first step status" ("event: step-status" `BS.isInfixOf` firstReplay && "\"projectId\":1" `BS.isInfixOf` firstReplay)
    (secondReplay, _) <- nextEvent "second replayed step status" afterFirstReplay
    assertBool "a later connection replays step statuses in order" ("\"projectId\":2" `BS.isInfixOf` secondReplay)

projectsJson :: LBS.ByteString
projectsJson =
    mconcat
        [ "{"
        , "\"1\":{\"id\":1,\"children\":[{\"step\":{\"id\":7,\"hidden\":true,\"sortKey\":null,\"def\":{\"id\":7}}}]},"
        , "\"2\":{\"id\":2,\"children\":[{\"project\":{\"id\":1,\"hidden\":false,\"sortKey\":null}},{\"step\":{\"id\":7,\"hidden\":false,\"sortKey\":null,\"def\":{\"id\":7}}}]},"
        , "\"3\":{\"id\":3,\"children\":[{\"step\":{\"id\":8,\"hidden\":false,\"sortKey\":null,\"def\":{\"id\":8}}},{\"project\":{\"id\":7,\"hidden\":false,\"sortKey\":null}}]}"
        , "}"
        ]

nextEvent :: String -> IO (StepT IO BS.ByteString) -> IO (BS.ByteString, IO (StepT IO BS.ByteString))
nextEvent label stream = do
    pulled <- timeout 2000000 (pullStep stream)
    case pulled of
        Nothing -> fail (label ++ " did not arrive")
        Just Nothing -> fail ("stream ended before " ++ label)
        Just (Just event) -> pure event

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
