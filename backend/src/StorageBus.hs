{-# LANGUAGE OverloadedStrings #-}

module StorageBus (
    StorageView (..),
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
) where

import ColdProvider (ColdStatus, OffloadProgress (..))
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTVar, readTVarIO, stateTVar, writeTChan, writeTVar)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), encode, object, (.=))
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

data StorageView = StorageView
    { storageColdConfigured :: Bool
    , storageLocal :: Integer
    , storageCold :: Integer
    , storageFree :: Integer
    , storageDiskTotal :: Integer
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
            , "free" .= storageFree view
            , "diskTotal" .= storageDiskTotal view
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
    }
    deriving (Eq, Show)

instance Semigroup Invalidation where
    Invalidation headA outputsA coldA viewA <> Invalidation headB outputsB coldB viewB =
        Invalidation (headA || headB) (outputsA || outputsB) (coldA <> coldB) (viewA || viewB)

instance Monoid Invalidation where
    mempty = Invalidation False False RecheckNone False

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
revalidateAll = invalidate (Invalidation True True RecheckAll False)

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

publishView :: StorageView -> IO ()
publishView view = atomically $ do
    previous <- readTVar viewVar
    when (fmap withoutTime previous /= Just (withoutTime view)) $ do
        writeTVar viewVar (Just view)
        writeTChan viewChan view
  where
    withoutTime snapshot = snapshot{storageUpdatedAt = storageUpdatedAt view}

currentView :: STM (Maybe StorageView)
currentView = readTVar viewVar

viewAndSubscribe :: IO (Maybe StorageView, TChan StorageView)
viewAndSubscribe = atomically $ (,) <$> readTVar viewVar <*> dupTChan viewChan

encodeView :: StorageView -> LBS.ByteString
encodeView = encode
