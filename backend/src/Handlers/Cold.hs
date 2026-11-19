{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cold (
    ColdStatus (..),
    ColdFile (..),
    ColdFileState (..),
    coldFilesFor,
    requireColdFiles,
    refuseColdFile,
    refuseOffloadedIn,
    coldStatusHandler,
    onloadHandler,
    parseColdStatus,
) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (Eff, UnliftStrategy (SeqUnlift), withEffToIO, (:>))
import Effects (AppM, ColdStorage, coldStorageConfigured, coldStorageOnload, coldStorageStatus)
import Handlers.StepOutput (resolveStepOutPath)
import Servant (NoContent (..), ServerError (..), err409, err502, err503, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import System.FilePath (joinPath, normalise, splitDirectories)
import System.IO.Unsafe (unsafePerformIO)

newtype ColdStatus = ColdStatus {coldStatusFiles :: [ColdFile]}
    deriving (Show, Eq)

data ColdFile = ColdFile
    { coldFilePath :: Text
    , coldFileState :: ColdFileState
    }
    deriving (Show, Eq)

data ColdFileState
    = Offloaded
    | Onloading Integer Integer
    | OnloadFailed Text
    deriving (Show, Eq)

instance FromJSON ColdStatus where
    parseJSON = withObject "cold storage status" $ \o -> ColdStatus <$> o .: "files"

instance ToJSON ColdStatus where
    toJSON status = object ["files" .= coldStatusFiles status]

instance FromJSON ColdFile where
    parseJSON = withObject "cold storage file" $ \o -> do
        path <- o .: "path"
        state <- o .: "state"
        ColdFile path <$> case state :: Text of
            "offloaded" -> pure Offloaded
            "onloading" -> Onloading <$> o .: "done" <*> o .: "total"
            "failed" -> OnloadFailed <$> o .: "error"
            other -> fail ("unknown cold storage file state " ++ T.unpack other)

instance ToJSON ColdFile where
    toJSON file =
        object $
            ("path" .= coldFilePath file) : case coldFileState file of
                Offloaded -> ["state" .= ("offloaded" :: Text)]
                Onloading done total -> ["state" .= ("onloading" :: Text), "done" .= done, "total" .= total]
                OnloadFailed message -> ["state" .= ("failed" :: Text), "error" .= message]

parseColdStatus :: BS.ByteString -> Maybe ColdStatus
parseColdStatus = decodeStrict

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

coldFilesFor :: (ColdStorage :> es) => Text -> Eff es (Maybe (Set Text))
coldFilesFor outPath = do
    configured <- coldStorageConfigured
    if not configured
        then pure (Just Set.empty)
        else either (const Nothing) (fmap notLocalPaths . parseColdStatus) <$> coldStorageStatus outPath
  where
    notLocalPaths = Set.fromList . map (T.pack . normalise . T.unpack . coldFilePath) . coldStatusFiles

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
        then pure (ColdStatus [])
        else do
            outPath <- coldOutPath stepId mCommit
            result <- lift (coldStorageStatus outPath)
            maybe
                (throwError err503{errBody = "Cannot read the cold storage status. Try again when storage status is available."})
                pure
                (either (const Nothing) parseColdStatus result)

onloadHandler :: Int -> Maybe Text -> FilePath -> AppM NoContent
onloadHandler stepId mCommit rel = do
    configured <- lift coldStorageConfigured
    if not configured
        then throwError err503{errBody = "Cold storage is not configured."}
        else do
            outPath <- coldOutPath stepId mCommit
            result <- lift (coldStorageOnload outPath rel)
            either (\message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 message)}) (const (pure NoContent)) result
