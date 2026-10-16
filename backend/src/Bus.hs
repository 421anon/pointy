{-# LANGUAGE OverloadedStrings #-}

module Bus (ProjectSnapshot (..), broadcastSnapshot, subscribe) where

import ClusterBus (buildingStepsAt)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import RollupCache (invalidateRollupCache)
import System.IO.Unsafe (unsafePerformIO)

data ProjectSnapshot = ProjectSnapshot
    { encoded :: LBS.ByteString
    }
    deriving (Show)

{-# NOINLINE statusBus #-}
statusBus :: TChan ProjectSnapshot
statusBus = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE recentSnapshots #-}
recentSnapshots :: TVar [ProjectSnapshot]
recentSnapshots = unsafePerformIO $ newTVarIO []

replayLimit :: Int
replayLimit = 256

{-# NOINLINE broadcastStates #-}
broadcastStates :: TVar (Map Text (Map Int Text))
broadcastStates = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE broadcastStateOrder #-}
broadcastStateOrder :: TVar [Text]
broadcastStateOrder = unsafePerformIO $ newTVarIO []

broadcastStateLimit :: Int
broadcastStateLimit = 16

broadcastSnapshot :: Int -> Text -> Map Int Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastSnapshot pid c certified reported = do
    changed <- atomically $ do
        building <- buildingStepsAt c
        let stats = Map.filterWithKey (\sid (st, _) -> Set.notMember sid building || st `elem` map pack ["running", "success"]) reported
            certifiedStats = Map.intersection certified stats
            snapshot = ProjectSnapshot (encodeSnapshot pid c certifiedStats stats)
            reportedStates = Map.map fst stats
        writeTChan statusBus snapshot
        modifyTVar' recentSnapshots (take replayLimit . (snapshot :))
        states <- readTVar broadcastStates
        order <- readTVar broadcastStateOrder
        let previous = Map.findWithDefault Map.empty c states
            differs = Map.foldrWithKey (\sid st acc -> acc || Map.lookup sid previous /= Just st) False reportedStates
            order' = take broadcastStateLimit (c : filter (/= c) order)
        writeTVar broadcastStates $ Map.restrictKeys (Map.insert c (Map.union reportedStates previous) states) (Set.fromList order')
        writeTVar broadcastStateOrder order'
        pure differs
    when changed invalidateRollupCache

subscribe :: IO (TChan ProjectSnapshot)
subscribe = atomically $ do
    chan <- dupTChan statusBus
    recent <- readTVar recentSnapshots
    mapM_ (unGetTChan chan) recent
    pure chan

encodeSnapshot :: Int -> Text -> Map Int Text -> Map Int (Text, Maybe Text) -> LBS.ByteString
encodeSnapshot projectId targetCommit certificates statuses =
    encode
        ( object
            [ "projectId" .= projectId
            , "commit" .= targetCommit
            , "steps"
                .= map
                    ( \(sid, (st, mErr)) ->
                        object
                            ( ["stepId" .= sid, "status" .= st]
                                ++ maybe [] (\e -> ["error" .= e]) mErr
                                ++ maybe [] (\c -> ["certificate" .= c]) (Map.lookup sid certificates)
                            )
                    )
                    (Map.toList statuses)
            ]
        )
