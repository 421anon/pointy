{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildRunner (JobComment (..), SlurmJob, encodeJobComment, parseSlurmJobLine)
import ClusterBus (ClusterSnapshot (..), ClusterStatus (..), FinishedStep (..), JobProgress (..), StepActivity (..), StepOutcome (..), StepPhase (..), TrackedBuild (..))
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Handlers.ClusterStream (clusterStatusFromAvailability, deriveJobProgress, encodeSnapshot)

main :: IO ()
main = do
    assertEqual
        "a down node is degraded and reports no usable nodes"
        (Degraded, Just "trotter down; no usable nodes")
        (clusterStatusFromAvailability (Right downNodeTranscript))
    assertEqual
        "idle nodes on up partitions are available"
        (Available, Nothing)
        (clusterStatusFromAvailability (Right "pointy*|up|node1|idle\npointy*|up|node2|allocated\n"))
    assertEqual
        "a drain among idle nodes is degraded"
        (Degraded, Just "node2 drain")
        (clusterStatusFromAvailability (Right "pointy*|up|node1|idle\npointy*|up|node2|drain\n"))
    assertEqual
        "a flag is named when the state alone does not explain the node"
        (Degraded, Just "node1 idle (not responding); no usable nodes")
        (clusterStatusFromAvailability (Right "pointy*|up|node1|idle*\n"))
    assertEqual
        "a flag is dropped when the state already explains the node"
        (Degraded, Just "trotter down; no usable nodes")
        (clusterStatusFromAvailability (Right "pointy*|up|trotter|down*\n"))
    assertEqual
        "a down partition is degraded"
        (Degraded, Just "partition debug down")
        (clusterStatusFromAvailability (Right "pointy*|up|node1|idle\ndebug|down|node1|idle\n"))
    assertEqual
        "impaired nodes beyond the limit collapse into a count"
        (Degraded, Just "node1 down, node2 down, node3 down, +1 more; no usable nodes")
        ( clusterStatusFromAvailability $
            Right "pointy*|up|node1|down\npointy*|up|node2|down\npointy*|up|node3|down\npointy*|up|node4|down\n"
        )
    assertEqual
        "an unreachable controller is unavailable with the error"
        (Unavailable, Just "slurm_load_partitions: Unable to contact slurm controller")
        (clusterStatusFromAvailability (Left "slurm_load_partitions: Unable to contact slurm controller\n(use -v to see more details)\n"))
    assertEqual
        "a cluster without nodes is unavailable"
        (Unavailable, Just "no compute nodes registered")
        (clusterStatusFromAvailability (Right ""))
    assertBool
        "degraded payload carries status and detail"
        ( "\"status\":\"degraded\"" `occursIn` degradedPayload
            && "\"detail\":\"trotter down; no usable nodes\"" `occursIn` degradedPayload
        )
    assertBool
        "available payload carries a null detail"
        ("\"detail\":null" `occursIn` payload (Available, Nothing))
    assertEqual
        "a running job started its phase its elapsed time ago, days included"
        (Just (JobProgress "41" Running (addUTCTime (negate 93784) now) Nothing))
        (Map.lookup 7 (deriveJobProgress now (jobs ["41|" ++ jobName ++ "|" ++ stepComment 7 commitA ++ "|RUNNING|1-02:03:04|None"]) buildsAtA Map.empty))
    assertEqual
        "a pending job is queued since its build was tracked, with the Slurm reason"
        (Just (JobProgress "42" Queued trackedAt (Just "Resources")))
        (Map.lookup 7 (deriveJobProgress now (jobs ["42|" ++ jobName ++ "|" ++ stepComment 7 commitA ++ "|PENDING|0:00|Resources"]) buildsAtA Map.empty))
    assertEqual
        "a job for a commit the step is not building at is ignored"
        Nothing
        (Map.lookup 7 (deriveJobProgress now (jobs ["43|" ++ jobName ++ "|" ++ stepComment 7 "other" ++ "|RUNNING|5:00|None"]) buildsAtA Map.empty))
    assertEqual
        "a running step keeps the start it was first seen with"
        (Just earlierStart)
        ( progressSince
            <$> Map.lookup
                7
                ( deriveJobProgress
                    now
                    (jobs ["41|" ++ jobName ++ "|" ++ stepComment 7 commitA ++ "|RUNNING|0:10|None"])
                    buildsAtA
                    (Map.singleton 7 (JobProgress "41" Running earlierStart Nothing))
                )
        )
    assertEqual
        "a step that leaves the queue starts its running phase from Slurm's elapsed time"
        (Just (JobProgress "41" Running (addUTCTime (-10) now) Nothing))
        ( Map.lookup
            7
            ( deriveJobProgress
                now
                (jobs ["41|" ++ jobName ++ "|" ++ stepComment 7 commitA ++ "|RUNNING|0:10|None"])
                buildsAtA
                (Map.singleton 7 (JobProgress "41" Queued trackedAt (Just "Priority")))
            )
        )
    assertBool
        "activity payload carries phase, reason and recent outcomes"
        ( "\"phase\":\"queued\"" `occursIn` activityPayload
            && "\"reason\":\"Dependency\"" `occursIn` activityPayload
            && "\"since\":\"2026-10-09T12:00:00Z\"" `occursIn` activityPayload
            && "\"outcome\":\"failed\"" `occursIn` activityPayload
        )

now :: UTCTime
now = timestamp "2026-10-09T13:00:00Z"

trackedAt :: UTCTime
trackedAt = timestamp "2026-10-09T12:00:00Z"

earlierStart :: UTCTime
earlierStart = timestamp "2026-10-09T12:30:00Z"

timestamp :: String -> UTCTime
timestamp text = maybe (error text) id (iso8601ParseM text)

commitA :: String
commitA = "f1c5ec2080eeed56e424178207fd8310081de368"

jobName :: String
jobName = "pointy-nix-build-nix-store-abc-step-0000000000000001"

stepComment :: Int -> String -> String
stepComment step commit = encodeJobComment (JobComment "step" step commit "/nix/store/abc-step")

jobs :: [String] -> [SlurmJob]
jobs = mapMaybe parseSlurmJobLine

buildsAtA :: Map.Map Int TrackedBuild
buildsAtA = Map.singleton 7 (TrackedBuild trackedAt (Map.singleton "f1c5ec2080eeed56e424178207fd8310081de368" 1))

activityPayload :: BS.ByteString
activityPayload =
    LBS.toStrict $
        encodeSnapshot
            ClusterSnapshot
                { clusterStatus = Available
                , clusterDetail = Nothing
                , activeSteps = Map.singleton 7 (StepActivity Queued trackedAt (Just "Dependency") ["f1c5ec2080eeed56e424178207fd8310081de368"])
                , recentSteps = [FinishedStep 8 Failed now (Just "dependency step(s) [3] could not be scheduled")]
                }

downNodeTranscript :: String
downNodeTranscript =
    "pointy*|up|trotter|down\ndebug|up|trotter|down\n"

degradedPayload :: BS.ByteString
degradedPayload = payload (Degraded, Just "trotter down; no usable nodes")

payload :: (ClusterStatus, Maybe Text) -> BS.ByteString
payload (status, detail) =
    LBS.toStrict (encodeSnapshot (ClusterSnapshot status detail Map.empty []))

occursIn :: BS.ByteString -> BS.ByteString -> Bool
occursIn = BS.isInfixOf

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
