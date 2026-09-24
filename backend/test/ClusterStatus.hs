{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ClusterBus (ClusterSnapshot (..), ClusterStatus (..))
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Text (Text)
import Handlers.ClusterStream (clusterStatusFromAvailability, encodeSnapshot)

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

downNodeTranscript :: String
downNodeTranscript =
    "pointy*|up|trotter|down\ndebug|up|trotter|down\n"

degradedPayload :: BS.ByteString
degradedPayload = payload (Degraded, Just "trotter down; no usable nodes")

payload :: (ClusterStatus, Maybe Text) -> BS.ByteString
payload (status, detail) =
    LBS.toStrict (encodeSnapshot (ClusterSnapshot status detail (Set.singleton 7)))

occursIn :: BS.ByteString -> BS.ByteString -> Bool
occursIn = BS.isInfixOf

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
