{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cold (
    coldFilesFor,
    coldStatusFor,
    mapColdProvider,
    onloadable,
    requireColdFiles,
    refuseColdFile,
    refuseOffloadedIn,
    clearRefusalsAndBroadcast,
    coldStatusHandler,
    onloadHandler,
    startOnload,
) where

import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..), parseColdStatus)
import ColdRefusal (clearColdRefusalsForPath)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift, SeqUnlift), liftIO, withEffToIO, (:>))
import Effects (AppM, ColdStorage, coldStorageConfigured, coldStorageOnload, coldStorageStatus)
import Handlers.Statuses (broadcastStatusForStepProjects)
import Handlers.StepOutput (resolveStepOutPath)
import Servant (NoContent (..), ServerError (..), err400, err409, err502, err503, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import StorageBus (clearOffloadError, recheckCold, recordColdStatus)
import System.FilePath (joinPath, normalise, splitDirectories)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE coldOutPaths #-}
coldOutPaths :: SingleFlightCache (Int, Text) Text
coldOutPaths = unsafePerformIO (newSingleFlightCache 256)

coldOutPath :: Int -> Maybe Text -> AppM Text
coldOutPath stepId mCommit = case mCommit of
    Nothing -> resolveStepOutPath stepId Nothing
    Just commit ->
        ExceptT $
            withEffToIO SeqUnlift $ \unlift ->
                cachedSuccess coldOutPaths (stepId, commit) (unlift (runExceptT (resolveStepOutPath stepId (Just commit))))

coldFilesFor :: (ColdStorage :> es, IOE :> es) => Text -> Eff es (Maybe (Set Text))
coldFilesFor outPath = fmap notLocalPaths <$> coldStatusFor outPath
  where
    notLocalPaths = Set.fromList . map (T.pack . normalise . T.unpack . coldFilePath) . filter (not . isOffloading) . coldStatusFiles
    isOffloading file = case coldFileState file of
        Offloading _ _ -> True
        _ -> False

coldStatusFor :: (ColdStorage :> es, IOE :> es) => Text -> Eff es (Maybe ColdStatus)
coldStatusFor outPath = do
    configured <- coldStorageConfigured
    if not configured
        then pure (Just (ColdStatus [] Nothing Nothing))
        else do
            status <- either (const Nothing) parseColdStatus <$> coldStorageStatus outPath
            liftIO (recordColdStatus outPath status)
            pure status

clearRefusalsAndBroadcast :: Text -> IO ()
clearRefusalsAndBroadcast outPath = do
    cleared <- clearColdRefusalsForPath outPath
    forM_ cleared $ \(stepId, commit) ->
        runAppEffects (broadcastStatusForStepProjects stepId commit Nothing)

startOnload :: (ColdStorage :> es, IOE :> es) => Text -> FilePath -> Eff es (Either Text ())
startOnload outPath rel = do
    liftIO (clearOffloadError outPath)
    result <- coldStorageOnload outPath rel
    case result of
        Right () -> liftIO (followOnloads outPath)
        Left _ -> pure ()
    liftIO (recheckCold outPath)
    pure result

{-# NOINLINE onloadWatchersRef #-}
onloadWatchersRef :: MVar (Set Text)
onloadWatchersRef = unsafePerformIO (newMVar Set.empty)

followOnloads :: Text -> IO ()
followOnloads outPath = do
    watching <- modifyMVar onloadWatchersRef (\watchers -> pure (Set.insert outPath watchers, Set.notMember outPath watchers))
    when watching $ do
        _ <- forkIO (watchUnavailable 0)
        pure ()
  where
    watchUnavailable unavailableChecks = do
        threadDelay 1000000
        status <- runAppEffects (coldStatusFor outPath)
        case status of
            Nothing
                | unavailableChecks + 1 >= 15 -> stopWatching
                | otherwise -> watchUnavailable (unavailableChecks + 1)
            Just cold
                | all (notOnloading . coldFileState) (coldStatusFiles cold) -> do
                    stopWatching
                    when (null (coldStatusFiles cold)) $ do
                        clearRefusalsAndBroadcast outPath
                        clearOffloadError outPath
                    recheckCold outPath
                | otherwise -> watchUnavailable 0
    stopWatching = modifyMVar_ onloadWatchersRef (pure . Set.delete outPath)
    notOnloading Onloading{} = False
    notOnloading _ = True

onloadable :: ColdFile -> Bool
onloadable file = case coldFileState file of
    Offloading _ _ -> False
    Onloading _ _ -> False
    _ -> True

{-# NOINLINE coldProviderSemaphore #-}
coldProviderSemaphore :: QSem
coldProviderSemaphore = unsafePerformIO (newQSem 8)

mapColdProvider :: (IOE :> es) => (a -> Eff es b) -> [a] -> Eff es [b]
mapColdProvider action items =
    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
        mapConcurrently (bracket_ (waitQSem coldProviderSemaphore) (signalQSem coldProviderSemaphore) . unlift . action) items

offloadedEnclosing :: Set Text -> FilePath -> Maybe Text
offloadedEnclosing coldFiles rel = find (`Set.member` coldFiles) enclosing
  where
    components = dropWhile (== "/") (splitDirectories (normalise rel))
    enclosing = [T.pack (joinPath (take n components)) | n <- [1 .. length components]]

requireColdFiles :: Maybe (Set Text) -> AppM (Set Text)
requireColdFiles =
    maybe (throwError err503{errBody = "Cannot check whether this file is offloaded to cold storage. Try again when storage status is available."}) pure

refuseColdFile :: Text -> FilePath -> AppM ()
refuseColdFile storePath rel = do
    coldFiles <- requireColdFiles =<< lift (coldFilesFor storePath)
    refuseOffloadedIn coldFiles rel

refuseOffloadedIn :: Set Text -> FilePath -> AppM ()
refuseOffloadedIn coldFiles rel =
    forM_ (offloadedEnclosing coldFiles rel) $ \_ ->
        throwError err409{errBody = "This file is frozen in cold storage. Rewarm it, then try again."}

coldStatusHandler :: Int -> Maybe Text -> AppM ColdStatus
coldStatusHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    if not configured
        then pure (ColdStatus [] Nothing Nothing)
        else do
            outPath <- coldOutPath stepId mCommit
            maybe
                (throwError err503{errBody = "Cannot read the cold storage status. Try again when storage status is available."})
                pure
                =<< lift (coldStatusFor outPath)

onloadHandler :: Int -> Maybe Text -> FilePath -> AppM NoContent
onloadHandler stepId mCommit rel = do
    configured <- lift coldStorageConfigured
    if not configured
        then throwError err400{errBody = "Cold storage is not configured."}
        else do
            outPath <- coldOutPath stepId mCommit
            result <- lift (startOnload outPath rel)
            either (\message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 message)}) (const (pure NoContent)) result
