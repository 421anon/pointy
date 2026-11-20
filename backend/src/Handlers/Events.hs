{-# LANGUAGE OverloadedStrings #-}

module Handlers.Events (eventsHandler, eventsStream) where

import qualified Bus
import qualified ClusterBus
import ClusterStatusReport (encodeSnapshot)
import Control.Concurrent.STM (orElse, readTChan)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Effects (AppM)
import qualified IngestBus
import qualified Servant.Types.SourceT as S
import Sse (SseResponse, broadcastLoop, sseEvent, sseResponse)

eventsHandler :: AppM SseResponse
eventsHandler = liftIO (sseResponse <$> eventsStream)

eventsStream :: IO (S.StepT IO BS.ByteString)
eventsStream = do
    stepStatuses <- Bus.subscribe
    (cluster, clusterUpdates) <- ClusterBus.snapshotAndSubscribe
    (ingestJobs, ingestUpdates) <- IngestBus.snapshotAndSubscribe
    let nextEvent =
            (stepStatusEvent <$> readTChan stepStatuses)
                `orElse` (clusterStatusEvent <$> readTChan clusterUpdates)
                `orElse` (ingestJobsEvent <$> readTChan ingestUpdates)
    pure $
        foldr
            (S.Yield . uncurry sseEvent)
            (S.Effect (broadcastLoop nextEvent))
            [clusterStatusEvent cluster, ingestJobsEvent ingestJobs]
  where
    stepStatusEvent snapshot = ("step-status", Bus.encoded snapshot)
    clusterStatusEvent snapshot = ("cluster-status", encodeSnapshot snapshot)
    ingestJobsEvent jobs = ("ingest-jobs", IngestBus.encodeJobs jobs)
