{-# LANGUAGE OverloadedStrings #-}

module StorageBus (
    StorageView (..),
    StorageRevision (..),
    StorageStep (..),
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
    stepSummaries,
    revisionMovable,
) where

import ColdProvider (ColdStatus, OffloadProgress (..))
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChanIO, newTVarIO, readTVar, readTVarIO, stateTVar, writeTChan, writeTVar)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (maximumBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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

data StorageStep = StorageStep
    { storageStepId :: Int
    , storageStepName :: Text
    , storageStepLocal :: Integer
    , storageStepCold :: Integer
    , storageStepRevisionCount :: Int
    , storageStepLocalRevisionCount :: Int
    , storageStepBlocked :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON StorageStep where
    toJSON step =
        object
            [ "step" .= storageStepId step
            , "name" .= storageStepName step
            , "local" .= storageStepLocal step
            , "cold" .= storageStepCold step
            , "revisionCount" .= storageStepRevisionCount step
            , "localRevisionCount" .= storageStepLocalRevisionCount step
            , "blocked" .= storageStepBlocked step
            ]

stepSummaries :: [StorageRevision] -> [StorageStep]
stepSummaries revisions = sortOn summaryOrder (map summarize (Map.elems grouped))
  where
    grouped = Map.fromListWith (<>) [(revisionStep revision, [revision]) | revision <- revisions]
    summarize group =
        StorageStep
            { storageStepId = stepId
            , storageStepName = name
            , storageStepLocal = sum (map revisionLocal group)
            , storageStepCold = sum (map revisionCold group)
            , storageStepRevisionCount = length group
            , storageStepLocalRevisionCount = length (filter revisionMovable group)
            , storageStepBlocked = blocked
            }
      where
        newest = maximumBy (comparing (\revision -> (revisionCommittedAt revision, revisionCommit revision))) group
        stepId = revisionStep newest
        newestName = revisionName newest
        name = if T.null newestName then "Step " <> T.pack (show stepId) else newestName
        blocked = case [reason | revision <- group, Just reason <- [revisionBlocked revision]] of
            reason : _ -> Just reason
            [] -> Nothing
    summaryOrder summary =
        ( negate (storageStepLocal summary)
        , negate (storageStepCold summary)
        , storageStepId summary
        )

revisionMovable :: StorageRevision -> Bool
revisionMovable revision = revisionState revision `elem` ["local", "failed"]

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
    , storageSteps :: [StorageStep]
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
            , "steps" .= storageSteps view
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
