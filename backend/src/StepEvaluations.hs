module StepEvaluations (
    CachedStep (..),
    StepEvaluations,
    EvaluationRecord,
    emptyStepEvaluations,
    recordEvaluation,
    evaluatedCommit,
    commitEvaluation,
    internedStep,
    encodeEvaluationRecords,
    decodeEvaluationLog,
    loadStepEvaluations,
    appendEvaluationRecords,
    writeStepEvaluations,
    stepEvaluationsPath,
) where

import Control.Exception (onException)
import Control.Monad (replicateM, unless)
import Data.Binary.Get (Get, getByteString, getInt32le, getWord32le, getWord8, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putInt32le, putWord32le, putWord8, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (mapAccumL, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as VU
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadWriteMode), SeekMode (SeekFromEnd), hFileSize, hSeek, hSetFileSize, withBinaryFile)

data CachedStep = CachedStep
    { cachedStepName :: !Text
    , cachedStepPath :: !(Maybe Text)
    , cachedStepKind :: !Text
    }
    deriving (Eq, Ord, Show)

data StepEvaluations = StepEvaluations
    { internedSteps :: !(IntMap CachedStep)
    , internedStepIds :: !(Map CachedStep Int)
    , commitEvaluations :: !(Map Text (VU.Vector (Int32, Int32)))
    }
    deriving (Eq, Show)

data EvaluationRecord
    = InternedStep CachedStep
    | EvaluatedCommit Text (VU.Vector (Int32, Int32))
    deriving (Eq, Show)

emptyStepEvaluations :: StepEvaluations
emptyStepEvaluations = StepEvaluations IntMap.empty Map.empty Map.empty

recordEvaluation :: Text -> Map Int CachedStep -> StepEvaluations -> (StepEvaluations, [EvaluationRecord])
recordEvaluation commit steps evaluations = (foldl' applyRecord evaluations records, records)
  where
    (newSteps, entries) = mapAccumL intern Map.empty (Map.toAscList steps)
    intern pending (stepId, step) = case Map.lookup step (internedStepIds evaluations) of
        Just existing -> (pending, (fromIntegral stepId, fromIntegral existing))
        Nothing -> case Map.lookup step pending of
            Just fresh -> (pending, (fromIntegral stepId, fromIntegral fresh))
            Nothing ->
                let fresh = IntMap.size (internedSteps evaluations) + Map.size pending
                 in (Map.insert step fresh pending, (fromIntegral stepId, fromIntegral fresh))
    records =
        map (InternedStep . fst) (sortOn snd (Map.toList newSteps))
            ++ [EvaluatedCommit commit (VU.fromList entries)]

applyRecord :: StepEvaluations -> EvaluationRecord -> StepEvaluations
applyRecord evaluations record = case record of
    InternedStep step ->
        let fresh = IntMap.size (internedSteps evaluations)
         in evaluations
                { internedSteps = IntMap.insert fresh step (internedSteps evaluations)
                , internedStepIds = Map.insert step fresh (internedStepIds evaluations)
                }
    EvaluatedCommit commit entries -> evaluations{commitEvaluations = Map.insert commit entries (commitEvaluations evaluations)}

evaluatedCommit :: Text -> StepEvaluations -> Bool
evaluatedCommit commit = Map.member commit . commitEvaluations

commitEvaluation :: Text -> StepEvaluations -> Maybe (VU.Vector (Int32, Int32))
commitEvaluation commit = Map.lookup commit . commitEvaluations

internedStep :: StepEvaluations -> Int32 -> CachedStep
internedStep evaluations stepRef = internedSteps evaluations IntMap.! fromIntegral stepRef

encodeEvaluationRecords :: [EvaluationRecord] -> BL.ByteString
encodeEvaluationRecords = runPut . mapM_ putRecord

putRecord :: EvaluationRecord -> Put
putRecord record = case record of
    InternedStep step -> do
        putWord8 0
        putText (cachedStepName step)
        maybe (putWord8 0) (\path -> putWord8 1 >> putText path) (cachedStepPath step)
        putText (cachedStepKind step)
    EvaluatedCommit commit entries -> do
        putWord8 1
        putText commit
        putWord32le (fromIntegral (VU.length entries))
        VU.mapM_ (\(stepId, stepRef) -> putInt32le stepId >> putInt32le stepRef) entries

putText :: Text -> Put
putText text = do
    let bytes = TE.encodeUtf8 text
    putWord32le (fromIntegral (BS.length bytes))
    putByteString bytes

getRecord :: Get EvaluationRecord
getRecord = do
    tag <- getWord8
    case tag of
        0 -> do
            name <- getText
            hasPath <- getWord8
            path <- case hasPath of
                0 -> pure Nothing
                1 -> Just <$> getText
                _ -> fail "invalid step path marker"
            InternedStep . CachedStep name path <$> getText
        1 -> do
            commit <- getText
            count <- fromIntegral <$> getWord32le
            EvaluatedCommit commit . VU.fromListN count <$> replicateM count ((,) <$> getInt32le <*> getInt32le)
        _ -> fail "invalid record tag"

getText :: Get Text
getText = do
    size <- fromIntegral <$> getWord32le
    either (fail . show) pure . TE.decodeUtf8' =<< getByteString size

decodeEvaluationLog :: BL.ByteString -> (StepEvaluations, Int64)
decodeEvaluationLog = go emptyStepEvaluations 0
  where
    go evaluations consumed remaining
        | BL.null remaining = (evaluations, consumed)
        | otherwise = case runGetOrFail getRecord remaining of
            Left _ -> (evaluations, consumed)
            Right (rest, used, record) -> go (applyRecord evaluations record) (consumed + used) rest

stepEvaluationsPath :: IO FilePath
stepEvaluationsPath = do
    home <- getHomeDirectory
    pure (home </> ".local" </> "state" </> "pointy" </> "storage" </> "evaluations.bin")

loadStepEvaluations :: IO StepEvaluations
loadStepEvaluations = do
    path <- stepEvaluationsPath
    present <- doesFileExist path
    if not present
        then pure emptyStepEvaluations
        else do
            bytes <- BS.readFile path
            let (evaluations, valid) = decodeEvaluationLog (BL.fromStrict bytes)
            unless (valid == fromIntegral (BS.length bytes)) $
                withBinaryFile path ReadWriteMode (`hSetFileSize` fromIntegral valid)
            pure evaluations

appendEvaluationRecords :: [EvaluationRecord] -> IO ()
appendEvaluationRecords records = do
    path <- stepEvaluationsPath
    createDirectoryIfMissing True (takeDirectory path)
    withBinaryFile path ReadWriteMode $ \handle -> do
        size <- hFileSize handle
        hSeek handle SeekFromEnd 0
        BL.hPut handle (encodeEvaluationRecords records) `onException` hSetFileSize handle size

writeStepEvaluations :: [EvaluationRecord] -> IO FilePath
writeStepEvaluations records = do
    path <- stepEvaluationsPath
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile (path ++ ".tmp") (encodeEvaluationRecords records)
    renameFile (path ++ ".tmp") path
    pure path
