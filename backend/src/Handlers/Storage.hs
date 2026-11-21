{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Storage (
    offloadHandler,
    offloadStepHandler,
    runStorageCenter,
    storageHandler,
) where

import BuildRunner (JobComment (..), SlurmJob (..), decodeJobComment, isRunningState, parseSlurmJobLine)
import ClusterBus (latestSlurmJobs)
import ColdProvider (ColdStatus, OffloadProgress (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, retry, writeTVar)
import Control.Exception (SomeException, catch, displayException, try)
import Control.Monad (join, unless, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict', encode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, liftIO, (:>))
import Effects (AppEffects, AppM, Eval, Nix, SlurmQuery (..), coldStorageConfigured, coldStorageOffload, querySlurm, runNixCli)
import Handlers.Cold (coldStatusFor, mapColdProvider)
import Handlers.StepOutput (resolveStepOutPath)
import NixEvaluator (EvalFailure (..), historyEvaluatorWorkers)
import NixStore (rootedPath)
import Processes (cli)
import Servant (NoContent (..), ServerError (..), err400, err409, err503)
import StorageBus (ColdRecheck (..), Invalidation (..), OffloadState (..), StorageRevision (..), StorageView (..), beginOffload, currentView, endOffload, invalidate, knownColdStatuses, offloadStates, publishView, recordOffloadProgress, revalidateAll, revisionMovable, stepSummaries, takeInvalidation, trackPaths, viewInputsChanged)
import StorageHistory (CachedStep (..), ColdSummary (..), HistoryCommit (..), RevisionSeed (..), dependentsOf, parseHistory, revisionsFromHistory, summarizeCold)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UserRepo (commitContext, commitContextEff, runGitIn, runHistoryEvalJsonApplyInRepo, runNixEvalJsonApplyInRepo, userRepoPath)

data HeadStep = HeadStep
    { headStepName :: Text
    , headStepDeps :: Set Int
    }
    deriving (Eq, Show)

instance FromJSON HeadStep where
    parseJSON = withObject "head step" $ \fields ->
        HeadStep
            <$> fields .:? "name" .!= ""
            <*> (Set.fromList <$> fields .:? "deps" .!= [])

newtype PathInfo = PathInfo {pathNarSize :: Integer}

instance FromJSON PathInfo where
    parseJSON = withObject "path info" $ \fields -> PathInfo <$> fields .: "narSize"

newtype EvalEntry = EvalEntry (Int, CachedStep)

instance FromJSON EvalEntry where
    parseJSON = withObject "step evaluation" $ \fields -> do
        stepId <- fields .: "id"
        name <- fields .:? "name" .!= ""
        path <- fields .:? "path"
        pure (EvalEntry (stepId, CachedStep name path))

newtype EvaluationFile = EvaluationFile (Map Text (Map Int CachedStep))

instance FromJSON EvaluationFile where
    parseJSON = withObject "storage evaluations" $ \fields -> do
        commits <- fields .: "commits" :: Parser (Map Text (Map Text CachedStep))
        pure . EvaluationFile . Map.map (Map.fromList . mapMaybe numbered . Map.toList) $ commits
      where
        numbered (key, step) = (\stepId -> (stepId, step)) <$> readMaybe (T.unpack key)

instance ToJSON EvaluationFile where
    toJSON (EvaluationFile commits) =
        object
            [ "commits" .= Map.map (Map.mapKeys (T.pack . show)) commits
            ]

data OffloadRequestError = OffloadBlocked Text | OffloadUnavailable Text

data Caches = Caches
    { cachedHead :: Maybe Text
    , cachedRelevant :: [(Text, UTCTime)]
    , cachedHeadSteps :: Either Text (Map Int HeadStep)
    , cachedSizes :: Map Text (Maybe Integer)
    , cachedHistoryError :: Maybe Text
    }

storageHistoryLimit :: Int
storageHistoryLimit = 5000

scanRetryDelaySeconds :: Int
scanRetryDelaySeconds = 60

persistEvery :: Int
persistEvery = 50

pathInfoChunkSize :: Int
pathInfoChunkSize = 200

maxShownRevisions :: Int
maxShownRevisions = 200

coalesceMicros :: Int
coalesceMicros = 500000

{-# NOINLINE cachesVar #-}
cachesVar :: TVar Caches
cachesVar = unsafePerformIO (newTVarIO (Caches Nothing [] (Left "The repository has not been read yet.") Map.empty Nothing))

{-# NOINLINE evaluationsRef #-}
evaluationsRef :: MVar (Maybe (Map Text (Map Int CachedStep)))
evaluationsRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE movablePathsVar #-}
movablePathsVar :: TVar (Map Int (Set Text))
movablePathsVar = unsafePerformIO (newTVarIO Map.empty)

data ScanState = ScanState
    { scanRunning :: Bool
    , scanUnavailable :: Maybe (UTCTime, Text)
    }

data ScanQueue = ScanQueue
    { queuePending :: [Text]
    , queueFailure :: Maybe (UTCTime, Text)
    }

{-# NOINLINE scanStateRef #-}
scanStateRef :: MVar ScanState
scanStateRef = unsafePerformIO (newMVar (ScanState False Nothing))

storageHandler :: AppM StorageView
storageHandler = liftIO $ do
    revalidateAll
    atomically (currentView >>= maybe retry pure)

offloadHandler :: Int -> Maybe Text -> AppM NoContent
offloadHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    unless configured $ throwError err400{errBody = "Cold storage is not configured."}
    refusal <- offloadRefusal stepId
    case refusal of
        Just (OffloadBlocked reason) -> throwError err409{errBody = LBS.fromStrict (TE.encodeUtf8 reason)}
        Just (OffloadUnavailable reason) -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 reason)}
        Nothing -> pure ()
    outPath <- resolveStepOutPath stepId mCommit
    started <- liftIO (beginOffload outPath)
    when started $ liftIO $ void $ forkIO $ do
        outcome <- try (runAppEffects (coldStorageOffload outPath (recordOffloadProgress outPath)))
        endOffload outPath (either (\(err :: SomeException) -> Left (T.pack (displayException err))) id outcome)
    pure NoContent

offloadStepHandler :: Int -> AppM NoContent
offloadStepHandler stepId = do
    configured <- lift coldStorageConfigured
    unless configured $ throwError err400{errBody = "Cold storage is not configured."}
    refusal <- offloadRefusal stepId
    case refusal of
        Just (OffloadBlocked reason) -> throwError err409{errBody = LBS.fromStrict (TE.encodeUtf8 reason)}
        Just (OffloadUnavailable reason) -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 reason)}
        Nothing -> pure ()
    paths <- liftIO (Map.findWithDefault Set.empty stepId <$> readTVarIO movablePathsVar)
    unless (Set.null paths) $
        liftIO $ void $ forkIO $ mapM_ offloadPath (Set.toList paths)
    pure NoContent
  where
    offloadPath outPath = do
        started <- beginOffload outPath
        when started $ do
            outcome <- try (runAppEffects (coldStorageOffload outPath (recordOffloadProgress outPath)))
            endOffload outPath (either (\(err :: SomeException) -> Left (T.pack (displayException err))) id outcome)

offloadRefusal :: Int -> AppM (Maybe OffloadRequestError)
offloadRefusal stepId = do
    headSteps <- cachedHeadSteps <$> liftIO (readTVarIO cachesVar)
    running <- runningSteps
    pure $ case (headSteps, running) of
        (Left err, _) -> Just (OffloadUnavailable err)
        (_, Left err) -> Just (OffloadUnavailable err)
        (Right steps, Right states) -> OffloadBlocked <$> runningRefusal steps states stepId

runningSteps :: AppM (Either Text (Map Int Text))
runningSteps = do
    result <- lift (querySlurm AllJobs)
    pure $ case result of
        Left err -> Left (T.pack err)
        Right output -> Right (runningStepStates (mapMaybe parseSlurmJobLine (lines output)))

runningStepStates :: [SlurmJob] -> Map Int Text
runningStepStates jobs =
    Map.fromList
        [ (jobCommentStep comment, T.pack (slurmJobState job))
        | job <- jobs
        , isRunningState (slurmJobState job)
        , Just comment <- [decodeJobComment =<< slurmJobComment job]
        , jobCommentKind comment == "step" || jobCommentKind comment == "extras"
        ]

runningRefusal :: Map Int HeadStep -> Map Int Text -> Int -> Maybe Text
runningRefusal headSteps running stepId =
    case sortOn fst (Map.toList (Map.restrictKeys running (Set.insert stepId dependents))) of
        [] -> Nothing
        (blocker, jobState) : _ ->
            let stateWord = if jobState `elem` ["PENDING", "CONFIGURING"] then "queued" else "running"
             in if blocker == stepId
                    then Just ("This step is " <> stateWord <> ".")
                    else Just ("Step " <> T.pack (show blocker) <> " (" <> stepName blocker <> ") is " <> stateWord <> " and reads this output.")
  where
    dependents = dependentsOf (Map.map headStepDeps headSteps) stepId
    stepName blocker = case Map.lookup blocker headSteps of
        Just step | not (T.null (headStepName step)) -> headStepName step
        _ -> "step " <> T.pack (show blocker)

runStorageCenter :: IO ()
runStorageCenter = do
    invalidate mempty{invalidHead = True}
    loop Map.empty
  where
    loop previousRunning = do
        (work, running) <- atomically $ do
            work <- takeInvalidation
            running <- runningStepStates <$> latestSlurmJobs
            when (work == mempty && running == previousRunning) retry
            pure (work, running)
        outcome <- try (runAppEffects (refresh work running))
        either reportFailure pure outcome
        threadDelay coalesceMicros
        loop running
    reportFailure (err :: SomeException) = do
        putStrLn ("Storage center refresh failed: " ++ displayException err)
        now <- getCurrentTime
        published <- atomically currentView
        when (isNothing published) $
            publishView
                False
                ( StorageView
                    { storageColdConfigured = False
                    , storageLocal = 0
                    , storageCold = 0
                    , storageFree = 0
                    , storageDiskTotal = 0
                    , storageScanning = False
                    , storageScanned = 0
                    , storageHistory = 0
                    , storageTruncated = False
                    , storageRevisionCount = 0
                    , storageRevisions = []
                    , storageSteps = []
                    , storageError = Just "Storage usage is unavailable. The server could not read it."
                    , storageUpdatedAt = now
                    }
                )

refresh :: Invalidation -> Map Int Text -> Eff AppEffects ()
refresh work running = do
    repoPath <- liftIO userRepoPath
    previous <- liftIO (readTVarIO cachesVar)
    caches <-
        if invalidHead work || isNothing (cachedHead previous)
            then refreshHistory repoPath previous
            else pure previous
    evaluations <- liftIO readEvaluations
    let relevant = cachedRelevant caches
        seeds = revisionsFromHistory [HistoryCommit commit time (Map.findWithDefault Map.empty commit evaluations) | (commit, time) <- relevant]
        scannedCount = length [() | (commit, _) <- relevant, Map.member commit evaluations]
        pending = [commit | (commit, _) <- relevant, Map.notMember commit evaluations]
        toScan = take (max 0 (storageHistoryLimit - scannedCount)) pending
        seedPaths = Set.fromList (map seedPath seeds)
        unknown = Set.filter (unknownSize (invalidOutputs work) (cachedSizes caches)) seedPaths
    liftIO (startScan repoPath toScan)
    sizeOutcome <- pathSizes (Set.toList unknown)
    let sizes = Map.restrictKeys (either (const (cachedSizes caches)) (\found -> Map.union (Map.fromSet (`Map.lookup` found) unknown) (cachedSizes caches)) sizeOutcome) seedPaths
        present = Map.mapMaybe id sizes
    liftIO $ atomically $ writeTVar cachesVar caches{cachedSizes = sizes}
    liftIO (trackPaths (Map.keysSet present))
    coldConfigured <- coldStorageConfigured
    known <- liftIO knownColdStatuses
    let recheck = case invalidCold work of
            RecheckAll -> Map.keysSet present
            RecheckPaths paths -> Set.intersection paths (Map.keysSet present)
            RecheckNone -> Set.empty
        toCheck = Set.union recheck (Map.keysSet (Map.difference present known))
    when coldConfigured $ void $ mapColdProvider coldStatusFor (Set.toList toCheck)
    colds <- liftIO knownColdStatuses
    offloads <- liftIO offloadStates
    (free, diskTotal) <- liftIO diskUsage
    now <- liftIO getCurrentTime
    scanFailure <- liftIO (fmap (fmap snd . scanUnavailable) (readMVar scanStateRef))
    let blockedReason = case cachedHeadSteps caches of
            Right headSteps -> runningRefusal headSteps running
            Left _ -> const (Just "Cannot tell which steps read this output until the current revision evaluates.")
        revisionsWithSeeds =
            [ (seed, storageRevision coldConfigured (Map.lookup (seedPath seed) colds) (Map.lookup (seedPath seed) offloads) (blockedReason (seedStep seed)) seed narSize)
            | seed <- seeds
            , Just narSize <- [Map.lookup (seedPath seed) present]
            ]
        revisions =
            sortOn
                (\revision -> (negate (revisionLocal revision), negate (revisionCold revision)))
                (map snd revisionsWithSeeds)
        movablePaths =
            Map.fromListWith Set.union
                [ (seedStep seed, Set.singleton (seedPath seed))
                | (seed, revision) <- revisionsWithSeeds
                , revisionMovable revision
                ]
        steps = stepSummaries revisions
    liftIO $ atomically $ writeTVar movablePathsVar movablePaths
    liftIO $
        publishView
            (invalidBroadcast work)
            StorageView
                { storageColdConfigured = coldConfigured
                , storageLocal = sum (map revisionLocal revisions)
                , storageCold = sum (map revisionCold revisions)
                , storageFree = free
                , storageDiskTotal = diskTotal
                , storageScanning = not (null toScan)
                , storageScanned = scannedCount
                , storageHistory = length relevant
                , storageTruncated = null toScan && not (null pending)
                , storageRevisionCount = length revisions
                , storageRevisions = take maxShownRevisions revisions
                , storageSteps = steps
                , storageError =
                    firstError
                        [ cachedHistoryError caches
                        , either Just (const Nothing) sizeOutcome
                        , either Just (const Nothing) (cachedHeadSteps caches)
                        , if null toScan
                            then Nothing
                            else fmap (\reason -> "Some revisions could not be evaluated yet and will be retried: " <> reason) scanFailure
                        ]
                , storageUpdatedAt = now
                }
  where
    firstError errors = case [message | Just message <- errors] of
        message : _ -> Just message
        [] -> Nothing

unknownSize :: Bool -> Map Text (Maybe Integer) -> Text -> Bool
unknownSize recheckAll sizes path = recheckAll || Map.notMember path sizes

storageRevision :: Bool -> Maybe (Maybe ColdStatus) -> Maybe OffloadState -> Maybe Text -> RevisionSeed -> Integer -> StorageRevision
storageRevision coldConfigured status offload blocked seed narSize =
    StorageRevision
        { revisionStep = seedStep seed
        , revisionName = if T.null (seedName seed) then "Step " <> T.pack (show (seedStep seed)) else seedName seed
        , revisionCommit = seedCommit seed
        , revisionCommittedAt = seedCommitTime seed
        , revisionLocal = coldSummaryLocal summary
        , revisionCold = coldSummaryCold summary
        , revisionState = coldSummaryState summary
        , revisionDone = coldSummaryDone summary
        , revisionTotal = coldSummaryTotal summary
        , revisionError = coldSummaryError summary
        , revisionBlocked = blocked
        }
  where
    provided
        | coldConfigured = summarizeCold narSize (join status)
        | otherwise = ColdSummary "local" narSize 0 0 0 Nothing
    summary = case offload of
        Just (OffloadRunning (OffloadProgress done total)) -> provided{coldSummaryState = "offloading", coldSummaryDone = done, coldSummaryTotal = total}
        Just (OffloadFailed message) -> provided{coldSummaryError = Just ("Moving to cold storage failed: " <> message)}
        Nothing -> provided

refreshHistory :: FilePath -> Caches -> Eff AppEffects Caches
refreshHistory repoPath caches = do
    headResult <- liftIO (repoHead repoPath)
    case headResult of
        Left err -> pure caches{cachedHistoryError = Just ("Cannot read the user repository's HEAD: " <> err)}
        Right headHash
            | Just headHash == cachedHead caches -> pure caches{cachedHistoryError = Nothing}
            | otherwise -> do
                history <- liftIO (gitHistory repoPath)
                case history of
                    Left err -> pure caches{cachedHistoryError = Just ("Cannot read the user repository's history: " <> err)}
                    Right entries -> do
                        headSteps <- runExceptT (evalHeadSteps repoPath headHash)
                        pure
                            caches
                                { cachedHead = Just headHash
                                , cachedRelevant = [(commit, time) | (commit, time, files) <- entries, commitAffectsOutputs files]
                                , cachedHeadSteps = first T.pack headSteps
                                , cachedHistoryError = Nothing
                                }

evalHeadSteps :: (Eval :> es, IOE :> es) => FilePath -> Text -> ExceptT String (Eff es) (Map Int HeadStep)
evalHeadSteps repoPath headHash = do
    ctx <- commitContextEff repoPath headHash
    output <- runNixEvalJsonApplyInRepo ctx headStepsExpression "#pointy.steps"
    either (throwError . ("Cannot decode step information: " ++)) pure (eitherDecodeStrict' (TE.encodeUtf8 (T.pack output)))

headStepsExpression :: String
headStepsExpression =
    unlines
        [ "steps: builtins.mapAttrs (id: s:"
        , "  let name = builtins.tryEval (s.def.name or (s.name or \"\"));"
        , "  in {"
        , "    name = if name.success && builtins.isString name.value then name.value else \"\";"
        , "    deps = builtins.map builtins.fromJSON (s.dependencies or [ ]);"
        , "  }) steps"
        ]

pathSizes :: (Nix :> es) => [Text] -> Eff es (Either Text (Map Text Integer))
pathSizes paths = go paths
  where
    go [] = pure (Right Map.empty)
    go remaining = do
        let (chunk, rest) = splitAt pathInfoChunkSize remaining
        (code, output, err) <- runNixCli (["--offline", "path-info", "--json"] ++ map T.unpack chunk)
        case code of
            ExitFailure _ -> pure (Left ("Cannot read store sizes: " <> T.strip (TE.decodeUtf8Lenient err)))
            ExitSuccess -> case eitherDecodeStrict' output of
                Left message -> pure (Left ("Cannot decode store sizes: " <> T.pack message))
                Right infos -> do
                    restSizes <- go rest
                    pure $ Map.union (Map.mapMaybe (fmap pathNarSize) (infos :: Map Text (Maybe PathInfo))) <$> restSizes

diskUsage :: IO (Integer, Integer)
diskUsage = do
    (code, output, _) <- cli "df" ["-B1", "--output=avail,size", rootedPath "/nix/store"]
    pure $ case code of
        ExitFailure _ -> (0, 0)
        ExitSuccess -> case reverse (lines output) of
            line : _ -> case words line of
                [free, total] | Just freeBytes <- readInteger free, Just totalBytes <- readInteger total -> (freeBytes, totalBytes)
                _ -> (0, 0)
            [] -> (0, 0)
  where
    readInteger text = case reads text of
        [(value, "")] -> Just value
        _ -> Nothing

repoHead :: FilePath -> IO (Either Text Text)
repoHead repoPath = do
    (code, output, err) <- runGitIn repoPath ["rev-parse", "--verify", "HEAD"]
    pure $ case code of
        ExitSuccess -> Right (T.strip (T.pack output))
        ExitFailure _ -> Left (T.pack err)

gitHistory :: FilePath -> IO (Either Text [(Text, UTCTime, [FilePath])])
gitHistory repoPath = do
    (code, output, err) <- runGitIn repoPath ["log", "--first-parent", "--format=%H %ct", "--name-only"]
    pure $ case code of
        ExitSuccess -> Right (parseHistory output)
        ExitFailure _ -> Left (T.pack err)

commitAffectsOutputs :: [FilePath] -> Bool
commitAffectsOutputs files =
    null files
        || any (`elem` globalPaths) files
        || any (\file -> any (`isPrefixOf` file) relevantPrefixes) files
  where
    globalPaths = ["flake.nix", "flake.lock", "pointy.nix", "main.pointy"]
    relevantPrefixes = ["steps/", "srcFiles/", "templates/", "packages/"]

startScan :: FilePath -> [Text] -> IO ()
startScan _ [] = pure ()
startScan repoPath pending = do
    now <- getCurrentTime
    shouldStart <- modifyMVar scanStateRef $ \state ->
        let retryBlocked = case scanUnavailable state of
                Just (failedAt, _) -> diffUTCTime now failedAt < fromIntegral scanRetryDelaySeconds
                Nothing -> False
         in if scanRunning state || retryBlocked
                then pure (state, False)
                else pure (state{scanRunning = True}, True)
    when shouldStart $
        void $
            forkIO $ do
                outcome <- try (scanCommits repoPath pending)
                unavailable <- case outcome of
                    Right unavailable -> pure unavailable
                    Left (err :: SomeException) -> do
                        failedAt <- getCurrentTime
                        pure (Just (failedAt, T.pack (displayException err)))
                modifyMVar_ scanStateRef $ \state -> pure state{scanRunning = False, scanUnavailable = unavailable}
                mapM_
                    ( \(_, reason) -> do
                        putStrLn ("Storage history scan paused: " ++ T.unpack reason)
                        void $ forkIO (threadDelay (scanRetryDelaySeconds * 1000000) >> viewInputsChanged)
                    )
                    unavailable
                viewInputsChanged

scanCommits :: FilePath -> [Text] -> IO (Maybe (UTCTime, Text))
scanCommits repoPath pending = do
    queue <- newMVar (ScanQueue pending Nothing)
    persistLock <- newMVar ()
    completed <- newIORef 0
    replicateConcurrently_ historyEvaluatorWorkers (scanWorker queue persistLock completed)
    persistEvaluations persistLock
    queueFailure <$> readMVar queue
  where
    scanWorker queue persistLock completed = do
        next <- takeScanCommit queue
        case next of
            Nothing -> pure ()
            Just commit -> do
                outcome <- try @SomeException (evaluateCommit repoPath commit)
                case outcome of
                    Left err -> stopScanQueue queue (T.pack (displayException err))
                    Right (Left failure) -> stopScanQueue queue (evalFailureMessage failure)
                    Right (Right steps) -> do
                        insertEvaluation commit steps
                        viewInputsChanged
                        count <- atomicModifyIORef' completed (\done -> let total = succ done in (total, total))
                        when (count `mod` persistEvery == 0) (persistEvaluations persistLock)
                        scanWorker queue persistLock completed

takeScanCommit :: MVar ScanQueue -> IO (Maybe Text)
takeScanCommit queue =
    modifyMVar queue $ \state ->
        case (queueFailure state, queuePending state) of
            (Nothing, commit : rest) -> pure (state{queuePending = rest}, Just commit)
            _ -> pure (state, Nothing)

stopScanQueue :: MVar ScanQueue -> Text -> IO ()
stopScanQueue queue reason = do
    now <- getCurrentTime
    modifyMVar_ queue $ \state -> pure state{queueFailure = Just (now, reason)}

insertEvaluation :: Text -> Map Int CachedStep -> IO ()
insertEvaluation commit steps =
    modifyMVar_ evaluationsRef $ \cached -> do
        current <- maybe loadEvaluations pure cached
        pure (Just (Map.insert commit steps current))

persistEvaluations :: MVar () -> IO ()
persistEvaluations lock =
    modifyMVar_ lock $ \_ -> do
        evaluations <- readEvaluations
        storeEvaluations evaluations

evalFailureMessage :: EvalFailure -> Text
evalFailureMessage failure = case failure of
    EvalError message -> "Evaluation error: " <> T.pack message
    EvalUnavailable message -> T.pack message

evaluateCommit :: FilePath -> Text -> IO (Either EvalFailure (Map Int CachedStep))
evaluateCommit repoPath commit = do
    context <- runExceptT (commitContext repoPath commit)
    case context of
        Left message -> pure (Left (EvalUnavailable message))
        Right ctx -> do
            outcome <- applyHistoryEval ctx stepEvaluationsExpression
            case outcome of
                Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
                Left (EvalError _) -> evaluateFallback ctx
                Right output -> pure (decodeStepEvaluations output)
  where
    applyHistoryEval ctx expression = runAppEffects (runHistoryEvalJsonApplyInRepo ctx expression "#pointy.steps")
    evaluateFallback ctx = do
        names <- applyHistoryEval ctx stepNamesExpression
        case names of
            Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
            Left (EvalError _) -> pure (Right Map.empty)
            Right output ->
                case decodeStepNames output of
                    Left failure -> pure (Left failure)
                    Right stepIds -> evaluateStepIds ctx stepIds Map.empty
    evaluateStepIds _ [] steps = pure (Right steps)
    evaluateStepIds ctx (stepId : rest) steps = do
        outcome <- applyHistoryEval ctx (stepEvaluationExpression [stepId])
        case outcome of
            Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
            Left (EvalError _) -> evaluateStepIds ctx rest (Map.insert stepId (CachedStep "" Nothing) steps)
            Right output ->
                case decodeStepEvaluations output of
                    Left failure -> pure (Left failure)
                    Right evaluated ->
                        let step = Map.findWithDefault (CachedStep "" Nothing) stepId evaluated
                         in evaluateStepIds ctx rest (Map.insert stepId step steps)

decodeStepEvaluations :: String -> Either EvalFailure (Map Int CachedStep)
decodeStepEvaluations output =
    case eitherDecodeStrict' (TE.encodeUtf8 (T.pack output)) :: Either String [EvalEntry] of
        Left message -> Left (EvalUnavailable ("Cannot decode step evaluations: " ++ message))
        Right entries -> Right (Map.fromList (map (\(EvalEntry entry) -> entry) entries))

decodeStepNames :: String -> Either EvalFailure [Int]
decodeStepNames output =
    case eitherDecodeStrict' (TE.encodeUtf8 (T.pack output)) :: Either String [Text] of
        Left message -> Left (EvalUnavailable ("Cannot decode step identifiers: " ++ message))
        Right names -> Right (mapMaybe (readMaybe . T.unpack) names)

stepEvaluationsExpression :: String
stepEvaluationsExpression =
    "steps: map " ++ stepEvaluationMapper ++ " (builtins.attrNames steps)"

stepNamesExpression :: String
stepNamesExpression = "steps: builtins.attrNames steps"

stepEvaluationExpression :: [Int] -> String
stepEvaluationExpression stepIds =
    "steps: map "
        ++ stepEvaluationMapper
        ++ " [ "
        ++ unwords (map (show . show) stepIds)
        ++ " ]"

stepEvaluationMapper :: String
stepEvaluationMapper =
    "(id:"
        ++ " let s = steps.${id};"
        ++ " path = builtins.tryEval (toString s.outPath);"
        ++ " name = builtins.tryEval (s.def.name or (s.name or \"\"));"
        ++ " in { id = builtins.fromJSON id;"
        ++ " name = if name.success && builtins.isString name.value then name.value else \"\";"
        ++ " path = if path.success then path.value else null; })"

readEvaluations :: IO (Map Text (Map Int CachedStep))
readEvaluations = modifyMVar evaluationsRef $ \cached -> do
    evaluations <- maybe loadEvaluations pure cached
    pure (Just evaluations, evaluations)

loadEvaluations :: IO (Map Text (Map Int CachedStep))
loadEvaluations = do
    path <- evaluationsPath
    present <- doesFileExist path
    if not present
        then pure Map.empty
        else do
            contents <- LBS.readFile path `catch` \(_ :: SomeException) -> pure LBS.empty
            pure $ case eitherDecodeStrict' (LBS.toStrict contents) of
                Right (EvaluationFile commits) -> commits
                Left _ -> Map.empty

storeEvaluations :: Map Text (Map Int CachedStep) -> IO ()
storeEvaluations evaluations = do
    path <- evaluationsPath
    directory <- storageDirectory
    createDirectoryIfMissing True directory
    LBS.writeFile (path ++ ".tmp") (encode (EvaluationFile evaluations))
    renameFile (path ++ ".tmp") path

evaluationsPath :: IO FilePath
evaluationsPath = do
    directory <- storageDirectory
    pure (directory </> "evaluations.json")

storageDirectory :: IO FilePath
storageDirectory = do
    home <- getHomeDirectory
    pure (home </> ".local" </> "state" </> "pointy" </> "storage")
