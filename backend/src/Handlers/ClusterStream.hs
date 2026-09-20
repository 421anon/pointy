{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ClusterStream (clusterStatusStreamHandler, startClusterPoller) where

import ClusterBus (ClusterSnapshot (..), ClusterStatus (..), setClusterStatus, snapshotAndSubscribe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Text (Text)
import EffectRunner (runAppEffects)
import Effectful (Eff, (:>))
import Effects (AppM, Slurm, clusterAvailability)
import Servant (Header, Headers, addHeader)
import qualified Servant.Types.SourceT as S
import qualified Sse

{- | Query SLURM via sinfo to determine cluster availability.
Returns Unavailable if sinfo is not reachable, Available if all
partitions report "up", Degraded otherwise.
-}
checkClusterStatus :: (Slurm :> es) => Eff es ClusterStatus
checkClusterStatus = do
    result <- clusterAvailability
    pure $ case result of
        Left _ -> Unavailable
        Right stdout ->
            let states = filter (not . null) (lines stdout)
             in if null states
                    then Unavailable
                    else
                        if all (== "up") states
                            then Available
                            else Degraded

{- | Synchronously check cluster status and store it, then fork a
background loop that re-checks every 30 seconds.
-}
startClusterPoller :: IO ()
startClusterPoller = do
    status <- runAppEffects checkClusterStatus
    setClusterStatus status
    void $ forkIO $ forever $ do
        threadDelay Sse.heartbeatDelayMicros
        status' <- runAppEffects checkClusterStatus
        setClusterStatus status'

clusterStatusStreamHandler ::
    AppM
        ( Headers
            '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text]
            (S.SourceT IO BS.ByteString)
        )
clusterStatusStreamHandler = do
    (initialSnapshot, busChan) <- liftIO snapshotAndSubscribe
    let source =
            S.fromStepT
                ( S.Yield
                    (Sse.sseEvent "cluster-status" (encodeSnapshot initialSnapshot))
                    (S.Effect (streamLoop busChan))
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

streamLoop :: TChan ClusterSnapshot -> IO (S.StepT IO BS.ByteString)
streamLoop = Sse.broadcastLoop ((,) "cluster-status" . encodeSnapshot)

encodeSnapshot :: ClusterSnapshot -> LBS.ByteString
encodeSnapshot snapshot =
    encode $
        object
            [ "status" .= statusText (clusterStatus snapshot)
            , "runningStepIds" .= Set.toList (runningStepIds snapshot)
            ]

statusText :: ClusterStatus -> Text
statusText Available = "available"
statusText Degraded = "degraded"
statusText Unavailable = "unavailable"
