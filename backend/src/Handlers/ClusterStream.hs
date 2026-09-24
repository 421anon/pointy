{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ClusterStream (clusterStatusFromAvailability, clusterStatusStreamHandler, encodeSnapshot, startClusterPoller) where

import ClusterBus (ClusterSnapshot (..), ClusterStatus (..), setClusterStatus, snapshotAndSubscribe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import EffectRunner (runAppEffects)
import Effectful (Eff, (:>))
import Effects (AppM, Slurm, clusterAvailability)
import Servant (Header, Headers, addHeader)
import qualified Servant.Types.SourceT as S
import qualified Sse

data ClusterRow = ClusterRow
    { rowPartition :: Text
    , rowPartitionState :: Text
    , rowNode :: Text
    , rowNodeState :: Text
    , rowNodeFlags :: Text
    }

checkClusterStatus :: (Slurm :> es) => Eff es (ClusterStatus, Maybe Text)
checkClusterStatus = clusterStatusFromAvailability <$> clusterAvailability

clusterStatusFromAvailability :: Either String String -> (ClusterStatus, Maybe Text)
clusterStatusFromAvailability = \case
    Left err -> (Unavailable, Just (failureDetail err))
    Right stdout -> case mapMaybe parseClusterRow (T.lines (T.pack stdout)) of
        [] -> (Unavailable, Just "no compute nodes registered")
        rows ->
            let nodes = uniqueBy rowNode rows
                partitions = uniqueBy rowPartition rows
                impairedNodes = filter (not . healthyNode) nodes
                impairedPartitions = filter (not . healthyPartition) partitions
                detail =
                    joinDetail
                        (nodeDetail (null (filter healthyNode nodes)) impairedNodes)
                        (partitionDetail impairedPartitions)
             in case detail of
                    Just text -> (Degraded, Just text)
                    Nothing -> (Available, Nothing)

startClusterPoller :: IO ()
startClusterPoller = do
    (status, detail) <- runAppEffects checkClusterStatus
    setClusterStatus status detail
    void $ forkIO $ forever $ do
        threadDelay Sse.heartbeatDelayMicros
        (status', detail') <- runAppEffects checkClusterStatus
        setClusterStatus status' detail'

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
            , "detail" .= clusterDetail snapshot
            , "runningStepIds" .= Set.toList (runningStepIds snapshot)
            ]

statusText :: ClusterStatus -> Text
statusText Available = "available"
statusText Degraded = "degraded"
statusText Unavailable = "unavailable"

parseClusterRow :: Text -> Maybe ClusterRow
parseClusterRow line = case T.splitOn "|" line of
    (partition : partitionState : node : nodeState : _rest)
        | not (T.null (T.strip node)) ->
            let stripped = T.strip nodeState
                base = T.dropWhileEnd (`elem` flagChars) stripped
                flags = T.takeWhileEnd (`elem` flagChars) stripped
             in Just
                    ClusterRow
                        { rowPartition = T.dropWhileEnd (== '*') (T.strip partition)
                        , rowPartitionState = T.toLower (T.strip partitionState)
                        , rowNode = T.strip node
                        , rowNodeState = T.toLower base
                        , rowNodeFlags = flags
                        }
    _ -> Nothing

healthyNode :: ClusterRow -> Bool
healthyNode row =
    rowNodeState row `elem` healthyNodeStates
        && T.null (T.filter flagImpairs (rowNodeFlags row))

healthyNodeStates :: [Text]
healthyNodeStates = ["idle", "alloc", "allocated", "mix", "mixed", "comp", "completing", "res", "reserved"]

healthyPartition :: ClusterRow -> Bool
healthyPartition row = T.null (rowPartitionState row) || rowPartitionState row == "up"

flagChars :: String
flagChars = "*~#!%@^-+$"

flagImpairs :: Char -> Bool
flagImpairs flag = flag `elem` ("*~#!%@^" :: String)

flagNote :: Char -> Text
flagNote = \case
    '*' -> "not responding"
    '~' -> "powered off"
    '#' -> "powering up"
    '!' -> "powering down"
    '%' -> "powering down"
    '@' -> "rebooting"
    '^' -> "reboot issued"
    _ -> "unavailable"

detailLimit :: Int
detailLimit = 3

joinDetail :: Maybe Text -> Maybe Text -> Maybe Text
joinDetail Nothing other = other
joinDetail detail Nothing = detail
joinDetail (Just first) (Just second) = Just (first <> "; " <> second)

nodeDetail :: Bool -> [ClusterRow] -> Maybe Text
nodeDetail noUsableNodes rows = case rows of
    [] -> Nothing
    _ ->
        Just $
            T.intercalate ", " (map describeNode (take detailLimit rows) <> overflowCount detailLimit rows)
                <> suffix
  where
    suffix
        | noUsableNodes = "; no usable nodes"
        | otherwise = ""

describeNode :: ClusterRow -> Text
describeNode row = rowNode row <> " " <> stateLabel <> note
  where
    stateLabel
        | T.null (rowNodeState row) = "unknown"
        | otherwise = rowNodeState row
    note
        | rowNodeState row `elem` healthyNodeStates = flagSuffix
        | otherwise = ""
    flagSuffix = case map flagNote (T.unpack (T.filter flagImpairs (rowNodeFlags row))) of
        (first : _) -> " (" <> first <> ")"
        [] -> ""

partitionDetail :: [ClusterRow] -> Maybe Text
partitionDetail rows = case rows of
    [] -> Nothing
    _ ->
        Just $
            "partition "
                <> T.intercalate ", " (map describePartition (take detailLimit rows) <> overflowCount detailLimit rows)
  where
    describePartition row = rowPartition row <> " " <> rowPartitionState row

overflowCount :: Int -> [a] -> [Text]
overflowCount limit rows
    | length rows > limit = ["+" <> T.pack (show (length rows - limit)) <> " more"]
    | otherwise = []

failureDetail :: String -> Text
failureDetail err = case filter (not . T.null) (map T.strip (T.lines (T.pack err))) of
    (line : _) -> T.take 200 line
    [] -> "sinfo failed"

uniqueBy :: Ord b => (a -> b) -> [a] -> [a]
uniqueBy key = Map.elems . Map.fromListWith (\_ first -> first) . map (\item -> (key item, item))
