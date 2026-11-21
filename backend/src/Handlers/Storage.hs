{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Storage (
    offloadHandler,
    runStorageCenter,
    storageHandler,
) where

import BuildRunner (JobComment (..), SlurmJob (..), decodeJobComment, isRunningState, parseSlurmJobLine)
import ClusterBus (latestSlurmJobs)
import ColdProvider (ColdStatus, OffloadProgress (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, retry, writeTVar)
import Control.Exception (SomeException, catch, displayException, try)
import Control.Monad (forM, forM_, join, unless, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict', encode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, liftIO, (:>))
import Effects (AppEffects, AppM, Eval, Nix, SlurmQuery (..), coldStorageConfigured, coldStorageOffload, querySlurm, runNixCli)
import Handlers.Cold (coldStatusFor, mapColdProvider)
import Handlers.StepOutput (resolveStepOutPath)
import NixStore (rootedPath)
import Processes (cli)
import Servant (NoContent (..), ServerError (..), err400, err409, err503)
import StorageBus (ColdRecheck (..), Invalidation (..), OffloadState (..), StorageRevision (..), StorageView (..), beginOffload, currentView, endOffload, invalidate, knownColdStatuses, offloadStates, publishView, recordOffloadProgress, revalidateAll, takeInvalidation, trackPaths, viewInputsChanged)
import StorageHistory (CachedStep (..), ColdSummary (..), HistoryCommit (..), RevisionSeed (..), dependentsOf, parseHistory, revisionsFromHistory, summarizeCold)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UserRepo (commitContextEff, runGitIn, runNixEvalJsonApplyInRepo, userRepoPath)

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
            [ "version" .= (1 :: Int)
            , "commits" .= Map.map (Map.mapKeys (T.pack . show)) commits
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

evaluationChunkSize :: Int
evaluationChunkSize = 40

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

{-# NOINLINE scanRef #-}
scanRef :: MVar Bool
scanRef = unsafePerformIO (newMVar False)

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
            publishView (StorageView False 0 0 0 0 False 0 0 False 0 [] (Just "Storage usage is unavailable. The server could not read it.") now)

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
    let blockedReason = case cachedHeadSteps caches of
            Right headSteps -> runningRefusal headSteps running
            Left _ -> const (Just "Cannot tell which steps read this output until the current revision evaluates.")
        revisions =
            sortOn
                (\revision -> (negate (revisionLocal revision), negate (revisionCold revision)))
                [ storageRevision coldConfigured (Map.lookup (seedPath seed) colds) (Map.lookup (seedPath seed) offloads) (blockedReason (seedStep seed)) seed narSize
                | seed <- seeds
                , Just narSize <- [Map.lookup (seedPath seed) present]
                ]
    liftIO $
        publishView
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
                , storageError = firstError [cachedHistoryError caches, either Just (const Nothing) sizeOutcome, either Just (const Nothing) (cachedHeadSteps caches)]
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
    shouldStart <- modifyMVar scanRef $ \running -> pure (True, not running)
    when shouldStart $
        void $
            forkIO $ do
                outcome <- try (scanCommits repoPath pending)
                modifyMVar_ scanRef (const (pure False))
                case outcome of
                    Right () -> viewInputsChanged
                    Left (err :: SomeException) -> putStrLn ("Storage history scan failed: " ++ displayException err)

scanCommits :: FilePath -> [Text] -> IO ()
scanCommits repoPath pending = forM_ (chunksOf persistEvery pending) $ \batch -> do
    forM_ batch $ \commit -> do
        steps <- evaluateCommit repoPath commit
        modifyMVar_ evaluationsRef $ \cached -> do
            current <- maybe loadEvaluations pure cached
            pure (Just (Map.insert commit steps current))
        viewInputsChanged
    storeEvaluations =<< readEvaluations

evaluateCommit :: FilePath -> Text -> IO (Map Int CachedStep)
evaluateCommit repoPath commit = do
    stepIds <- stepIdsAt repoPath commit
    if null stepIds
        then pure Map.empty
        else Map.fromList . concat <$> mapM (evaluateChunk repoPath commit) (chunksOf evaluationChunkSize stepIds)

stepIdsAt :: FilePath -> Text -> IO [Int]
stepIdsAt repoPath commit = do
    (code, output, _) <- runGitIn repoPath ["ls-tree", "--name-only", "--end-of-options", T.unpack commit, "--", "steps"]
    pure $ case code of
        ExitFailure _ -> []
        ExitSuccess -> mapMaybe stepIdFromPath (lines output)
  where
    stepIdFromPath path
        | takeExtension path == ".nix" = readMaybe (takeBaseName path)
        | otherwise = Nothing

evaluateChunk :: FilePath -> Text -> [Int] -> IO [(Int, CachedStep)]
evaluateChunk repoPath commit stepIds = do
    outcome <- attemptEval (evalAtCommit repoPath commit stepIds)
    case outcome of
        Just entries -> pure entries
        Nothing -> do
            putStrLn ("Storage history scan could not evaluate the steps at " ++ T.unpack commit ++ ".")
            stepsEvaluate <- attemptEval (stepNamesAtCommit repoPath commit)
            case stepsEvaluate of
                Just _ -> concat <$> forM stepIds evaluateOne
                Nothing -> pure (map unevaluated stepIds)
  where
    evaluateOne stepId = fromMaybe [unevaluated stepId] <$> attemptEval (evalAtCommit repoPath commit [stepId])
    unevaluated stepId = (stepId, CachedStep "" Nothing)

attemptEval :: ExceptT String (Eff AppEffects) a -> IO (Maybe a)
attemptEval action = do
    outcome <- try @SomeException (runAppEffects (runExceptT action))
    pure $ case outcome of
        Right (Right value) -> Just value
        _ -> Nothing

stepNamesAtCommit :: (Eval :> es, IOE :> es) => FilePath -> Text -> ExceptT String (Eff es) String
stepNamesAtCommit repoPath commit = do
    ctx <- commitContextEff repoPath commit
    runNixEvalJsonApplyInRepo ctx "steps: builtins.attrNames steps" "#pointy.steps"

evalAtCommit :: (Eval :> es, IOE :> es) => FilePath -> Text -> [Int] -> ExceptT String (Eff es) [(Int, CachedStep)]
evalAtCommit repoPath commit stepIds = do
    ctx <- commitContextEff repoPath commit
    output <- runNixEvalJsonApplyInRepo ctx (stepEvaluationExpression stepIds) "#pointy.steps"
    either (throwError . ("Cannot decode step evaluations: " ++)) (pure . map (\(EvalEntry entry) -> entry)) (eitherDecodeStrict' (TE.encodeUtf8 (T.pack output)))

stepEvaluationExpression :: [Int] -> String
stepEvaluationExpression stepIds =
    "steps: map (id:"
        ++ " let s = steps.${id};"
        ++ " path = builtins.tryEval (toString s.outPath);"
        ++ " name = builtins.tryEval (s.def.name or (s.name or \"\"));"
        ++ " in { id = builtins.fromJSON id;"
        ++ " name = if name.success && builtins.isString name.value then name.value else \"\";"
        ++ " path = if path.success then path.value else null; }) "
        ++ "[ "
        ++ unwords (map (show . show) stepIds)
        ++ " ]"

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size items = let (chunk, rest) = splitAt size items in chunk : chunksOf size rest
