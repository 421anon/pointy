{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cold (
    coldFilesFor,
    coldStatusFor,
    mapColdProvider,
    onloadable,
    requireColdFiles,
    refuseColdFile,
    refuseOffloadedIn,
    coldStatusHandler,
    onloadHandler,
    startOnload,
) where

import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..), parseColdStatus)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift, SeqUnlift), liftIO, withEffToIO, (:>))
import Effects (AppM, ColdStorage, coldStorageConfigured, coldStorageOnload, coldStorageStatus)
import Handlers.StepOutput (resolveStepOutPath)
import Servant (NoContent (..), ServerError (..), err409, err502, err503, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import StorageBus (recheckCold, recordColdStatus)
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
    notLocalPaths = Set.fromList . map (T.pack . normalise . T.unpack . coldFilePath) . coldStatusFiles

coldStatusFor :: (ColdStorage :> es, IOE :> es) => Text -> Eff es (Maybe ColdStatus)
coldStatusFor outPath = do
    configured <- coldStorageConfigured
    if not configured
        then pure (Just (ColdStatus [] Nothing))
        else do
            status <- either (const Nothing) parseColdStatus <$> coldStorageStatus outPath
            liftIO (recordColdStatus outPath status)
            pure status

startOnload :: (ColdStorage :> es, IOE :> es) => Text -> FilePath -> Eff es (Either Text ())
startOnload outPath rel = do
    result <- coldStorageOnload outPath rel
    liftIO (recheckCold outPath)
    pure result

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
        throwError err409{errBody = "This file is offloaded to cold storage. Copy it back from its snowflake in the file browser, then try again."}

coldStatusHandler :: Int -> Maybe Text -> AppM ColdStatus
coldStatusHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    if not configured
        then pure (ColdStatus [] Nothing)
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
        then throwError err503{errBody = "Cold storage is not configured."}
        else do
            outPath <- coldOutPath stepId mCommit
            result <- lift (startOnload outPath rel)
            either (\message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 message)}) (const (pure NoContent)) result
