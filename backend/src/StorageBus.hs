{-# LANGUAGE OverloadedStrings #-}

module StorageBus (
    StorageView (..),
    StorageCapacity (..),
    parseStorageCapacity,
    StorageRevision (..),
    Invalidation (..),
    ColdRecheck (..),
    OffloadState (..),
    invalidate,
    takeInvalidation,
    headMoved,
    outputsBuilt,
    revalidateAll,
    recheckCold,
    viewInputsChanged,
    trackPaths,
    recordColdStatus,
    knownColdStatuses,
    beginOffload,
    recordOffloadProgress,
    endOffload,
    offloadStates,
    publishView,
    currentView,
    viewAndSubscribe,
    encodeView,
    revisionMovable,
) where

import ColdProvider (ColdStatus, OffloadProgress (..))
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTVar, readTVarIO, stateTVar, writeTChan, writeTVar)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import System.IO.Unsafe (unsafePerformIO)

data StorageRevision = StorageRevision
    { revisionStep :: Int
    , revisionName :: Text
    , revisionKind :: Text
    , revisionCommit :: Text
    , revisionCommittedAt :: UTCTime
    , revisionLocal :: Integer
    , revisionCold :: Integer
    , revisionState :: Text
    , revisionDone :: Integer
    , revisionTotal :: Integer
    , revisionError :: Maybe Text
    , revisionBlocked :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON StorageRevision where
    toJSON revision =
        object
            [ "step" .= revisionStep revision
            , "name" .= revisionName revision
            , "kind" .= revisionKind revision
            , "revision" .= revisionCommit revision
            , "committedAt" .= revisionCommittedAt revision
            , "local" .= revisionLocal revision
            , "cold" .= revisionCold revision
            , "state" .= revisionState revision
            , "done" .= revisionDone revision
            , "total" .= revisionTotal revision
            , "error" .= revisionError revision
            , "blocked" .= revisionBlocked revision
            ]

revisionMovable :: StorageRevision -> Bool
revisionMovable revision = revisionState revision `elem` ["local", "failed"]

data StorageCapacity = StorageCapacity
    { capacityKind :: Text
    , capacityLabel :: Text
    , capacityTotalBytes :: Integer
    , capacityUsedBytes :: Integer
    , capacityAvailableBytes :: Integer
    }
    deriving (Eq, Show)

instance ToJSON StorageCapacity where
    toJSON capacity =
        object
            [ "kind" .= capacityKind capacity
            , "label" .= capacityLabel capacity
            , "totalBytes" .= capacityTotalBytes capacity
            , "usedBytes" .= capacityUsedBytes capacity
            , "availableBytes" .= capacityAvailableBytes capacity
            ]

instance FromJSON StorageCapacity where
    parseJSON = withObject "storage capacity" $ \fields -> do
        totalBytes <- fields .: "totalBytes"
        unless (totalBytes > 0) $ fail "totalBytes must be positive"
        kind <- fields .: "kind"
        unless (kind `elem` (["filesystem", "quota"] :: [Text])) $ fail "kind must be filesystem or quota"
        used <- fields .:? "usedBytes" :: Parser (Maybe Integer)
        available <- fields .:? "availableBytes" :: Parser (Maybe Integer)
        (usedBytes, availableBytes) <- case (used, available) of
            (Just value, Nothing) ->
                let usedValue = max 0 value
                 in pure (usedValue, max 0 (totalBytes - usedValue))
            (Nothing, Just value) ->
                let availableValue = max 0 (min totalBytes value)
                 in pure (totalBytes - availableValue, availableValue)
            (Nothing, Nothing)
                | kind == "quota" -> pure (0, totalBytes)
            _ -> fail "requires exactly one of usedBytes and availableBytes, or only totalBytes for a quota"
        label <- fields .:? "label" .!= (if kind == "filesystem" then "disk" else "quota")
        pure
            StorageCapacity
                { capacityKind = kind
                , capacityLabel = label
                , capacityTotalBytes = totalBytes
                , capacityUsedBytes = usedBytes
                , capacityAvailableBytes = availableBytes
                }

parseStorageCapacity :: LBS.ByteString -> Either String StorageCapacity
parseStorageCapacity = eitherDecode

data StorageView = StorageView
    { storageColdConfigured :: Bool
    , storageLocal :: Integer
    , storageCold :: Integer
    , storageCapacity :: Maybe StorageCapacity
    , storageColdCapacity :: Maybe StorageCapacity
    , storageScanning :: Bool
    , storageScanned :: Int
    , storageHistory :: Int
    , storageTruncated :: Bool
    , storageRevisionCount :: Int
    , storageRevisions :: [StorageRevision]
    , storageError :: Maybe Text
    , storageUpdatedAt :: UTCTime
    }
    deriving (Eq, Show)

instance ToJSON StorageView where
    toJSON view =
        object
            [ "coldConfigured" .= storageColdConfigured view
            , "local" .= storageLocal view
            , "cold" .= storageCold view
            , "capacity" .= storageCapacity view
            , "coldCapacity" .= storageColdCapacity view
            , "scanning" .= storageScanning view
            , "scanned" .= storageScanned view
            , "history" .= storageHistory view
            , "truncated" .= storageTruncated view
            , "revisionCount" .= storageRevisionCount view
            , "revisions" .= storageRevisions view
            , "error" .= storageError view
            , "updatedAt" .= storageUpdatedAt view
            ]

data ColdRecheck = RecheckNone | RecheckPaths (Set Text) | RecheckAll
    deriving (Eq, Show)

instance Semigroup ColdRecheck where
    RecheckAll <> _ = RecheckAll
    _ <> RecheckAll = RecheckAll
    RecheckNone <> other = other
    other <> RecheckNone = other
    RecheckPaths left <> RecheckPaths right = RecheckPaths (Set.union left right)

instance Monoid ColdRecheck where
    mempty = RecheckNone

data Invalidation = Invalidation
    { invalidHead :: Bool
    , invalidOutputs :: Bool
    , invalidCold :: ColdRecheck
    , invalidView :: Bool
    , invalidBroadcast :: Bool
    }
    deriving (Eq, Show)

instance Semigroup Invalidation where
    Invalidation headA outputsA coldA viewA broadcastA <> Invalidation headB outputsB coldB viewB broadcastB =
        Invalidation (headA || headB) (outputsA || outputsB) (coldA <> coldB) (viewA || viewB) (broadcastA || broadcastB)

instance Monoid Invalidation where
    mempty = Invalidation False False RecheckNone False False

data OffloadState = OffloadRunning OffloadProgress | OffloadFailed Text
    deriving (Eq, Show)

{-# NOINLINE pendingVar #-}
pendingVar :: TVar Invalidation
pendingVar = unsafePerformIO (newTVarIO mempty)

{-# NOINLINE trackedVar #-}
trackedVar :: TVar (Set Text)
trackedVar = unsafePerformIO (newTVarIO Set.empty)

{-# NOINLINE coldVar #-}
coldVar :: TVar (Map Text (Maybe ColdStatus))
coldVar = unsafePerformIO (newTVarIO Map.empty)

{-# NOINLINE offloadsVar #-}
offloadsVar :: TVar (Map Text OffloadState)
offloadsVar = unsafePerformIO (newTVarIO Map.empty)

{-# NOINLINE viewVar #-}
viewVar :: TVar (Maybe StorageView)
viewVar = unsafePerformIO (newTVarIO Nothing)

{-# NOINLINE viewChan #-}
viewChan :: TChan StorageView
viewChan = unsafePerformIO newBroadcastTChanIO

invalidate :: Invalidation -> IO ()
invalidate = atomically . invalidateSTM

invalidateSTM :: Invalidation -> STM ()
invalidateSTM change = modifyTVar' pendingVar (<> change)

takeInvalidation :: STM Invalidation
takeInvalidation = stateTVar pendingVar (\pending -> (pending, mempty))

headMoved :: IO ()
headMoved = invalidate mempty{invalidHead = True}

outputsBuilt :: IO ()
outputsBuilt = invalidate mempty{invalidOutputs = True}

revalidateAll :: IO ()
revalidateAll = invalidate mempty{invalidHead = True, invalidOutputs = True, invalidCold = RecheckAll, invalidBroadcast = True}

recheckCold :: Text -> IO ()
recheckCold path = invalidate mempty{invalidCold = RecheckPaths (Set.singleton path)}

viewInputsChanged :: IO ()
viewInputsChanged = invalidate mempty{invalidView = True}

trackPaths :: Set Text -> IO ()
trackPaths paths = atomically $ do
    writeTVar trackedVar paths
    modifyTVar' coldVar (`Map.restrictKeys` paths)

recordColdStatus :: Text -> Maybe ColdStatus -> IO ()
recordColdStatus path status = atomically $ do
    tracked <- Set.member path <$> readTVar trackedVar
    known <- Map.lookup path <$> readTVar coldVar
    when (tracked && known /= Just status) $ do
        modifyTVar' coldVar (Map.insert path status)
        invalidateSTM mempty{invalidView = True}

knownColdStatuses :: IO (Map Text (Maybe ColdStatus))
knownColdStatuses = readTVarIO coldVar

beginOffload :: Text -> IO Bool
beginOffload path = atomically $ do
    offloads <- readTVar offloadsVar
    case Map.lookup path offloads of
        Just (OffloadRunning _) -> pure False
        _ -> do
            writeTVar offloadsVar (Map.insert path (OffloadRunning (OffloadProgress 0 0)) offloads)
            invalidateSTM mempty{invalidView = True}
            pure True

recordOffloadProgress :: Text -> OffloadProgress -> IO ()
recordOffloadProgress path progress = atomically $ do
    modifyTVar' offloadsVar (Map.insert path (OffloadRunning progress))
    invalidateSTM mempty{invalidView = True}

endOffload :: Text -> Either Text () -> IO ()
endOffload path outcome = atomically $ do
    modifyTVar' offloadsVar (Map.alter (const (either (Just . OffloadFailed) (const Nothing) outcome)) path)
    invalidateSTM mempty{invalidCold = RecheckPaths (Set.singleton path)}

offloadStates :: IO (Map Text OffloadState)
offloadStates = readTVarIO offloadsVar

publishView :: Bool -> StorageView -> IO ()
publishView broadcast view = atomically $ do
    previous <- readTVar viewVar
    let changed = fmap withoutTime previous /= Just (withoutTime view)
    writeTVar viewVar (Just view)
    when (broadcast || changed) (writeTChan viewChan view)
  where
    withoutTime snapshot = snapshot{storageUpdatedAt = storageUpdatedAt view}

currentView :: STM (Maybe StorageView)
currentView = readTVar viewVar

viewAndSubscribe :: IO (Maybe StorageView, TChan StorageView)
viewAndSubscribe = atomically $ (,) <$> readTVar viewVar <*> dupTChan viewChan

encodeView :: StorageView -> LBS.ByteString
encodeView = encode
