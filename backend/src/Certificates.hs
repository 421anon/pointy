{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Certificates (
    getProjectCertificates,
    getStepCertificate,
    evalProjectDefinitions,
    evalProjectDefinition,
    evaluatedProjectStepIds,
    cachedProjectDefinitions,
    decodeProjectDefinitions,
    checkRevision,
    withWriteRepoTransaction,
    rawStatusesFor,
    runningBuildKeys,
    isCertificateBuilding,
    ProjectDef (..),
) where

import BuildLog (StepStore, buildStepStore, rawStatusesBatched)
import BuildRunner (BuildKey (..), buildKeyForOutPath, querySlurmJobs, slurmJobName)
import BuildStatus (StepPaths (..), markBuiltOutputs)
import Bus (broadcastSnapshot)
import CertificateStore (lookupCertificate, lookupOutput, storeCertificate, storeOutput)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Object, Result (..), Value (..), decode, fromJSON, object, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bool (bool)
import Data.Either (isRight)
import Data.List (dropWhileEnd, find, intercalate, isPrefixOf, stripPrefix, tails)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effects (App, Eval, Nix, Slurm, runNixCli)
import ProjectTree (ChildRef (..), normalizeProject, normalizeProjects, projectStepEntries, stepEntries)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), RepoContext, WriteRepoContext, ensureRepoCommit, runNixEvalJsonApplyInRepo, userRepoPath, withReadRepoTransactionIO, withWriteRepoTransactionRaw)

data ProjectDef = ProjectDef
    { projectDefId :: Int
    , projectDefStepIds :: [Int]
    }
    deriving (Show)

instance FromJSON ProjectDef where
    parseJSON = withObject "ProjectDef" $ \fields -> do
        children <- fields .: "children"
        ProjectDef <$> fields .: "id" <*> pure [stepId | StepChild stepId <- children]

invalidCertificate :: FilePath
invalidCertificate = "/invalid"

schemaVersionWithKeys :: Int
schemaVersionWithKeys = 1

data Versioned a = Versioned
    { versionedSchema :: Int
    , versionedValue :: a
    }

instance (FromJSON a) => FromJSON (Versioned a) where
    parseJSON = withObject "Versioned" $ \fields ->
        Versioned <$> fields .: "version" <*> fields .: "value"

getProjectCertificates :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Map Int StepPaths))
getProjectCertificates pid targetCommit = runExceptT $ do
    ctx <- prepareCommit targetCommit
    declared <- declaredProjectStepIds ctx pid
    let ids = Set.toList (Set.fromList (versionedValue declared))
    paths <-
        if versionedSchema declared >= schemaVersionWithKeys
            then do
                keys <- resolveKeys ctx ids
                let known = [(sid, key) | (sid, Just key) <- Map.toList keys]
                liftIO $ scheduleRefreshIfMissing targetCommit (map snd known)
                resolveCertificates ctx known
            else
                if null ids
                    then pure Map.empty
                    else
                        withExceptT ("Failed to evaluate project certificates: " ++) $
                            fmap (Map.mapMaybe id) $
                                runJson ctx (projectAttr pid) legacyProjectCertificates
    pure $
        Map.union paths $
            Map.fromList [(sid, StepPaths invalidCertificate invalidCertificate) | sid <- ids, Map.notMember sid paths]

legacyProjectCertificates :: String
legacyProjectCertificates = "project: builtins.mapAttrs (id: certificate: if certificate == null then null else { inherit certificate; output = project.outPaths.${id} or certificate; }) (project.certificates or project.outPaths)"

resolveKeys :: (Eval :> es, IOE :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) (Map Int (Maybe Text))
resolveKeys _ [] = pure Map.empty
resolveKeys ctx ids = do
    outcome <- lift $ runExceptT $ runNixEvalJsonApplyInRepo ctx (stepKeysExpression ids) "#pointy"
    case outcome of
        Right output -> case decodeJson output of
            Just keys
                | Map.size (versionedValue keys) == Set.size (Set.fromList ids) -> pure (versionedValue keys)
            _ -> splitOrReportKeys ctx "the evaluation returned the wrong number of keys" ids
        Left err -> splitOrReportKeys ctx err ids

splitOrReportKeys :: (Eval :> es, IOE :> es) => ReadRepoContext -> String -> [Int] -> ExceptT String (Eff es) (Map Int (Maybe Text))
splitOrReportKeys ctx err ids = case ids of
    [single] -> do
        liftIO $ putStrLn $ "Step " ++ show single ++ " has no key: " ++ errorSummary err
        pure $ Map.singleton single Nothing
    _ -> do
        let (left, right) = splitAt (length ids `div` 2) ids
        Map.union <$> resolveKeys ctx left <*> resolveKeys ctx right

scheduleRefreshIfMissing :: Text -> [Text] -> IO ()
scheduleRefreshIfMissing commit keys = do
    settled <- refreshIsSettled commit
    when (not settled) $ do
        stored <- mapM lookupCertificate keys
        when (any isNothing stored) $ scheduleCertificateRefresh commit

getStepCertificate :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Maybe StepPaths))
getStepCertificate sid targetCommit = runExceptT $ do
    ctx <- prepareCommit targetCommit
    resolved <-
        withExceptT ("Failed to evaluate step key: " ++) $
            runJson ctx "#pointy" (stepKeyExpression sid)
    case versionedValue resolved of
        Just key | versionedSchema resolved >= schemaVersionWithKeys ->
            Map.lookup sid <$> resolveCertificates ctx [(sid, key)]
        _ -> do
            evaluated <- resolveCertificatePaths ctx [sid]
            pure $ case lookup sid evaluated of
                Just (Just path) -> Just path
                _ -> Nothing

resolveCertificates :: (Eval :> es, IOE :> es) => ReadRepoContext -> [(Int, Text)] -> ExceptT String (Eff es) (Map Int StepPaths)
resolveCertificates _ [] = pure Map.empty
resolveCertificates ctx keys = do
    stored <- liftIO $ forM keys $ \(sid, key) -> (,,) sid <$> lookupCertificate key <*> lookupOutput key
    let fromStore = Map.fromList [(sid, StepPaths certificate (fromMaybe certificate output)) | (sid, Just certificate, output) <- stored]
        missing = [sid | (sid, certificate, output) <- stored, isNothing certificate || isNothing output]
    if null missing
        then pure fromStore
        else do
            evaluated <- resolveCertificatePaths ctx missing
            let evaluatedPaths = Map.mapMaybe id (Map.fromList evaluated)
            liftIO $ storeNew keys (Map.toList evaluatedPaths)
            pure $ Map.union evaluatedPaths fromStore

resolveCertificatePaths :: (Eval :> es, IOE :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) [(Int, Maybe StepPaths)]
resolveCertificatePaths _ [] = pure []
resolveCertificatePaths ctx ids = do
    outcome <- lift $ runExceptT $ runNixEvalJsonApplyInRepo ctx (stepCertificatesExpression ids) "#pointy.steps"
    case outcome of
        Right output -> case decodeJson output of
            Just paths
                | length paths == length ids -> pure $ zip ids paths
            _ -> throwError "Failed to parse the certificate evaluation"
        Left err -> splitOrReport ctx err ids

splitOrReport :: (Eval :> es, IOE :> es) => ReadRepoContext -> String -> [Int] -> ExceptT String (Eff es) [(Int, Maybe StepPaths)]
splitOrReport ctx err ids = case ids of
    [single] -> do
        liftIO $ putStrLn $ "Step " ++ show single ++ " has no certificate: " ++ errorSummary err
        pure [(single, Nothing)]
    _ -> do
        let (left, right) = splitAt (length ids `div` 2) ids
        (++) <$> resolveCertificatePaths ctx left <*> resolveCertificatePaths ctx right

errorSummary :: String -> String
errorSummary text = case [rest | rest@(line : _) <- tails cleaned, "error:" `isPrefixOf` line] of
    [] -> fromMaybe "" (find (not . null) cleaned)
    matches -> describe (last matches)
  where
    cleaned = map (dropWhile (== ' ') . withoutStderrPrefix) (lines text)
    withoutStderrPrefix line = fromMaybe line (stripPrefix "stderr:" line)
    describe (line : next) = unwords (dropWhile (== ' ') (drop (length ("error:" :: String)) line) : location next)
    describe [] = ""
    location (next : _) | Just path <- stripPrefix "at " next = ["at " ++ storeRelative (dropWhileEnd (== ':') path)]
    location _ = []

storeRelative :: String -> String
storeRelative path = case [rest | rest <- tails path, "/nix/store/" `isPrefixOf` rest] of
    [] -> path
    found -> drop 1 (dropWhile (/= '-') (drop (length ("/nix/store/" :: String)) (last found)))

storeNew :: [(Int, Text)] -> [(Int, StepPaths)] -> IO ()
storeNew keys paths =
    forM_ paths $ \(sid, StepPaths certificate output) ->
        case lookup sid keys of
            Just key -> storeCertificate key certificate >> storeOutput key output
            Nothing -> pure ()

prepareCommit :: (Eval :> es, IOE :> es) => Text -> ExceptT String (Eff es) ReadRepoContext
prepareCommit targetCommit = do
    withExceptT ("Failed to prepare commit: " ++) $
        ExceptT $
            liftIO $
                runExceptT $
                    ensureRepoCommit $
                        unpack targetCommit
    repoPath <- liftIO userRepoPath
    pure $ ReadRepoContext repoPath (unpack targetCommit)

runJson :: (RepoContext ctx, Eval :> es, FromJSON a) => ctx -> String -> String -> ExceptT String (Eff es) a
runJson ctx attr applyExpr = do
    output <- withExceptT errorSummary (runNixEvalJsonApplyInRepo ctx applyExpr attr)
    maybe (throwError $ "Failed to parse the evaluation of " ++ attr) pure (decodeJson output)

data ProjectsEvaluation = ProjectsEvaluation
    { projectsValue :: Value
    , projectsFailures :: [EvaluationFailure]
    }

data EvaluationFailure = EvaluationFailure
    { failureSubject :: FailureSubject
    , failureCause :: String
    , failureMessage :: String
    }

data FailureSubject
    = RevisionProjects
    | ProjectSubject String
    | StepSubject Int
    deriving (Eq, Ord)

data ProjectProblem = ProjectProblem
    { problemCause :: String
    , problemMessage :: String
    }

data ProjectOutcome
    = ProjectEvaluated Value
    | ProjectDegraded Value [ProjectProblem]
    | ProjectFailed String

data EntryList
    = ChildEntries
    | LegacyStepEntries

evalProjectDefinitions :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) Value
evalProjectDefinitions ctx@(ReadRepoContext repoPath commit) = do
    evaluation <- evaluateProjectDefinitions ctx
    unless (null (projectsFailures evaluation)) $
        liftIO $
            reportDegradedProjects repoPath commit (map failureMessage (projectsFailures evaluation))
    pure (projectsValue evaluation)

reportDegradedProjects :: FilePath -> String -> [String] -> IO ()
reportDegradedProjects repoPath commit failures = do
    firstReport <- modifyMVar degradedProjectsReportedRef $ \reported ->
        pure (Set.insert (repoPath, commit) reported, Set.notMember (repoPath, commit) reported)
    when firstReport $
        putStrLn $
            "Projects at " ++ take 8 commit ++ " are served with evaluation failures: " ++ intercalate "; " failures

{-# NOINLINE degradedProjectsReportedRef #-}
degradedProjectsReportedRef :: MVar (Set.Set (FilePath, String))
degradedProjectsReportedRef = unsafePerformIO (newMVar Set.empty)

evaluateProjectDefinitions :: (RepoContext ctx, Eval :> es) => ctx -> ExceptT String (Eff es) ProjectsEvaluation
evaluateProjectDefinitions ctx =
    lift (runExceptT (runJson ctx projectsAttr projectDefinitions)) >>= \case
        Right projects -> pure (ProjectsEvaluation (normalizeProjects projects) [])
        Left _ -> do
            names <- runJson ctx projectsAttr "builtins.attrNames"
            outcomes <- lift $ mapM (evaluateProject ctx) names
            let entries = zip names outcomes
            pure
                ProjectsEvaluation
                    { projectsValue = normalizeProjects (toJSON (Map.fromList [(name, value) | (name, outcome) <- entries, Just value <- [presentedProject name outcome]]))
                    , projectsFailures = concatMap (uncurry projectOutcomeFailures) entries
                    }

evaluateProject :: (RepoContext ctx, Eval :> es) => ctx -> String -> Eff es ProjectOutcome
evaluateProject ctx name =
    runExceptT (runJson ctx attr projectDefinition) >>= \case
        Right value -> pure (ProjectEvaluated value)
        Left err ->
            runExceptT (runJson ctx attr projectHeaderDefinition) >>= \case
                Left headerErr -> pure (ProjectFailed headerErr)
                Right header -> do
                    (list, entries, failures) <- evaluateProjectEntries ctx attr
                    pure $ ProjectDegraded (withEvaluatedEntries list header entries failures) (if null failures then [ProjectProblem err err] else failures)
  where
    attr = projectsAttr ++ "." ++ name

evaluateProjectEntries :: (RepoContext ctx, Eval :> es) => ctx -> String -> Eff es (EntryList, [Value], [ProjectProblem])
evaluateProjectEntries ctx attr = do
    list <- either (const ChildEntries) (bool LegacyStepEntries ChildEntries) <$> runExceptT (runJson ctx attr "project: project ? children")
    runExceptT (runJson ctx attr ("project: builtins.length project." ++ entryListName list)) >>= \case
        Left err -> let message = "The " ++ entryListSubject list ++ " list does not evaluate: " ++ err in pure (list, [], [ProjectProblem message message])
        Right count -> (\(entries, failures) -> (list, entries, failures)) <$> evaluateEntryRange ctx attr list 0 count

evaluateEntryRange :: (RepoContext ctx, Eval :> es) => ctx -> String -> EntryList -> Int -> Int -> Eff es ([Value], [ProjectProblem])
evaluateEntryRange ctx attr list start count
    | count <= 0 = pure ([], [])
    | otherwise =
        runExceptT (runJson ctx attr (entryRangeExpression list start count)) >>= \case
            Right entries -> pure (entries, [])
            Left err
                | count == 1 -> do
                    entry <- failingEntry ctx attr list start
                    pure ([], [ProjectProblem err (entry ++ " does not evaluate: " ++ err)])
                | otherwise -> do
                    let half = count `div` 2
                    (<>) <$> evaluateEntryRange ctx attr list start half <*> evaluateEntryRange ctx attr list (start + half) (count - half)

entryRangeExpression :: EntryList -> Int -> Int -> String
entryRangeExpression list start count =
    "project: builtins.genList (i: builtins.elemAt project." ++ entryListName list ++ " (i + " ++ show start ++ ")) " ++ show count

failingEntry :: (RepoContext ctx, Eval :> es) => ctx -> String -> EntryList -> Int -> Eff es String
failingEntry ctx attr list index = case list of
    LegacyStepEntries -> pure ("Step at position " ++ position)
    ChildEntries ->
        either (const ("Child at position " ++ position)) (\stepId -> "Step " ++ show (stepId :: Int))
            <$> runExceptT (runJson ctx attr ("project: (builtins.elemAt project.children " ++ show index ++ ").step.id"))
  where
    position = show (index + 1)

entryListName :: EntryList -> String
entryListName ChildEntries = "children"
entryListName LegacyStepEntries = "steps"

entryListSubject :: EntryList -> String
entryListSubject ChildEntries = "child"
entryListSubject LegacyStepEntries = "step"

withEvaluatedEntries :: EntryList -> Object -> [Value] -> [ProjectProblem] -> Value
withEvaluatedEntries list header entries failures =
    Object $
        KeyMap.insert (Key.fromString (entryListName list)) (toJSON entries) $
            KeyMap.insert "validationErrors" (toJSON (declaredErrors ++ map problemMessage failures)) header
  where
    declaredErrors = case fromJSON <$> KeyMap.lookup "validationErrors" header of
        Just (Success errors) -> errors
        _ -> [] :: [String]

presentedProject :: String -> ProjectOutcome -> Maybe Value
presentedProject name = \case
    ProjectEvaluated value -> Just value
    ProjectDegraded value _ -> Just value
    ProjectFailed failure -> placeholderProject failure <$> readInt name

placeholderProject :: String -> Int -> Value
placeholderProject failure pid =
    object
        [ "id" .= pid
        , "name" .= ("Project " ++ show pid)
        , "preset" .= Null
        , "templates" .= ([] :: [String])
        , "children" .= ([] :: [Value])
        , "validationErrors" .= ["This project does not evaluate: " ++ failure]
        ]

projectOutcomeFailures :: String -> ProjectOutcome -> [EvaluationFailure]
projectOutcomeFailures name = \case
    ProjectEvaluated _ -> []
    ProjectDegraded _ problems -> [projectFailure (problemCause problem) (problemMessage problem) | problem <- problems]
    ProjectFailed failure -> [projectFailure failure failure]
  where
    projectFailure cause message = EvaluationFailure (ProjectSubject name) cause ("Project " ++ name ++ ": " ++ message)

evaluatedProjectStepIds :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) [Int]
evaluatedProjectStepIds ctx pid =
    lift (evaluateProject ctx (show pid)) >>= \case
        ProjectFailed failure -> throwError failure
        ProjectEvaluated value -> stepIdsOf value
        ProjectDegraded value _ -> stepIdsOf value
  where
    stepIdsOf value = case fromJSON (normalizeProject value) of
        Success project -> pure (projectDefStepIds project)
        Error err -> throwError err

declaredProjectStepIds :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) (Versioned [Int])
declaredProjectStepIds ctx pid =
    lift (runExceptT (runJson ctx "#pointy" (projectStepIdsExpression pid))) >>= \case
        Right declared -> pure declared
        Left err ->
            withExceptT (const ("Failed to evaluate project step ids: " ++ err)) $
                Versioned <$> runJson ctx "#pointy" schemaVersionExpression <*> evaluatedProjectStepIds ctx pid

checkRevision :: (Eval :> es) => ReadRepoContext -> ReadRepoContext -> [Int] -> ExceptT String (Eff es) ()
checkRevision target candidate stepIds = do
    candidateFailures <- lift (revisionFailures candidate stepIds)
    unless (null candidateFailures) $ do
        existing <- Set.fromList . map failureIdentity <$> lift (revisionFailures target stepIds)
        case [failureMessage failure | failure <- candidateFailures, Set.notMember (failureIdentity failure) existing] of
            [] -> pure ()
            introduced -> throwError (intercalate "\n" introduced)

failureIdentity :: EvaluationFailure -> (FailureSubject, String)
failureIdentity failure = (failureSubject failure, failureCause failure)

revisionFailures :: (Eval :> es) => ReadRepoContext -> [Int] -> Eff es [EvaluationFailure]
revisionFailures ctx stepIds = do
    projects <- runExceptT (evaluateProjectDefinitions ctx)
    stepFailures <- catMaybes <$> mapM (stepDefinitionFailure ctx) stepIds
    pure (either unevaluatedProjects projectsFailures projects ++ stepFailures)
  where
    unevaluatedProjects err = [EvaluationFailure RevisionProjects err ("Projects do not evaluate: " ++ err)]

stepDefinitionFailure :: (Eval :> es) => ReadRepoContext -> Int -> Eff es (Maybe EvaluationFailure)
stepDefinitionFailure ctx sid =
    either (Just . failure . errorSummary) (const Nothing)
        <$> runExceptT (runNixEvalJsonApplyInRepo ctx (stepDefinitionExpression sid) "#pointy")
  where
    failure cause = EvaluationFailure (StepSubject sid) cause ("Step " ++ show sid ++ " (steps/" ++ show sid ++ ".nix) does not evaluate: " ++ cause)

stepDefinitionExpression :: Int -> String
stepDefinitionExpression sid =
    "pointy: if pointy.steps ? " ++ show (show sid) ++ " then pointy.steps." ++ show (show sid) ++ ".def else null"

cachedProjectDefinitions :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) (Map String ProjectDef)
cachedProjectDefinitions ctx@(ReadRepoContext repoPath commit) = do
    cached <- liftIO $ lookupCachedProjectDefinitions repoPath commit
    case cached of
        Just definitions -> return definitions
        Nothing -> do
            projects <- evalProjectDefinitions ctx
            definitions <- either throwError return (decodeProjectDefinitions projects)
            liftIO $ insertCachedProjectDefinitions repoPath commit definitions
            return definitions

{-# NOINLINE projectDefinitionsCacheRef #-}
projectDefinitionsCacheRef :: MVar [((FilePath, String), Map String ProjectDef)]
projectDefinitionsCacheRef = unsafePerformIO (newMVar [])

projectDefinitionsCacheLimit :: Int
projectDefinitionsCacheLimit = 4

lookupCachedProjectDefinitions :: FilePath -> String -> IO (Maybe (Map String ProjectDef))
lookupCachedProjectDefinitions repoPath commit = do
    cache <- readMVar projectDefinitionsCacheRef
    return $ lookup (repoPath, commit) cache

insertCachedProjectDefinitions :: FilePath -> String -> Map String ProjectDef -> IO ()
insertCachedProjectDefinitions repoPath commit definitions = do
    modifyMVar_ projectDefinitionsCacheRef $ \entries ->
        return $ take projectDefinitionsCacheLimit $ ((repoPath, commit), definitions) : filter ((/= (repoPath, commit)) . fst) entries

evalProjectDefinition :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) String
evalProjectDefinition ctx pid = runNixEvalJsonApplyInRepo ctx projectDefinition (projectAttr pid)

decodeProjectDefinitions :: Value -> Either String (Map String ProjectDef)
decodeProjectDefinitions projects = case fromJSON projects of
    Success definitions -> Right definitions
    Error err -> Left ("Failed to parse " ++ projectsAttr ++ ": " ++ err)

projectsAttr :: String
projectsAttr = "#pointy.projects"

projectAttr :: Int -> String
projectAttr pid = projectsAttr ++ "." ++ show pid

projectDefinitions :: String
projectDefinitions = "builtins.mapAttrs (_: " ++ projectDefinition ++ ")"

projectDefinition :: String
projectDefinition = "project: builtins.removeAttrs project [ \"outPaths\" \"certificates\" ]"

projectHeaderDefinition :: String
projectHeaderDefinition = "project: builtins.removeAttrs project [ \"outPaths\" \"certificates\" \"steps\" \"children\" ]"

stepKeyExpression :: Int -> String
stepKeyExpression sid =
    "pointy: "
        ++ schemaVersionPreamble
        ++ "{ inherit version; value = "
        ++ keyGuard ("pointy.steps." ++ show (show sid) ++ ".key")
        ++ "; }"

stepKeysExpression :: [Int] -> String
stepKeysExpression ids =
    "pointy: "
        ++ schemaVersionPreamble
        ++ "{ inherit version; value = builtins.listToAttrs (map (id: { name = toString id; value = "
        ++ keyGuard "pointy.steps.${toString id}.key"
        ++ "; }) "
        ++ renderIdList ids
        ++ "); }"

stepIdsExpression :: String
stepIdsExpression =
    "pointy: " ++ schemaVersionPreamble ++ "{ inherit version; value = builtins.map (name: builtins.fromJSON name) (builtins.attrNames pointy.steps); }"

projectStepIdsExpression :: Int -> String
projectStepIdsExpression pid =
    "pointy: "
        ++ schemaVersionPreamble
        ++ "{ inherit version; value = builtins.map (s: s.def.id) "
        ++ projectStepEntries "pointy.projects" pid
        ++ "; }"

schemaVersionPreamble :: String
schemaVersionPreamble = "let version = let t = builtins.tryEval (pointy.schemaVersion or 0); in if t.success then t.value else 0; in "

schemaVersionExpression :: String
schemaVersionExpression = "pointy: " ++ schemaVersionPreamble ++ "version"

keyGuard :: String -> String
keyGuard selection =
    "if version >= "
        ++ show schemaVersionWithKeys
        ++ " then (let t = builtins.tryEval ("
        ++ selection
        ++ "); in if t.success then t.value else null) else null"

stepCertificatesExpression :: [Int] -> String
stepCertificatesExpression ids =
    "steps: map (id: let tryOutPath = drv: builtins.tryEval (toString drv.outPath); certificate = tryOutPath (steps.${id}.certificate or steps.${id}); output = tryOutPath steps.${id}; in if certificate.success then { certificate = certificate.value; output = (if output.success then output else certificate).value; } else null) "
        ++ renderIdList ids

renderIdList :: [Int] -> String
renderIdList ids = "[ " ++ unwords (map (show . show) ids) ++ " ]"

projectMembershipExpression :: String
projectMembershipExpression = "projects: builtins.mapAttrs (_: p: builtins.map (s: s.def.id) " ++ stepEntries "p" ++ ") projects"

certificateBatchSize :: Int
certificateBatchSize = 512

data RefreshState = RefreshState
    { refreshRunning :: Bool
    , refreshPending :: Maybe Text
    , refreshSettled :: Set.Set Text
    }

{-# NOINLINE refreshStateRef #-}
refreshStateRef :: MVar RefreshState
refreshStateRef = unsafePerformIO (newMVar (RefreshState False Nothing Set.empty))

refreshSettledRevisions :: Int
refreshSettledRevisions = 64

markRefreshSettled :: MVar RefreshState -> Text -> IO ()
markRefreshSettled ref commit =
    modifyMVar_ ref $ \state ->
        pure state{refreshSettled = Set.fromList (take refreshSettledRevisions (commit : Set.toList (refreshSettled state)))}

refreshIsSettled :: Text -> IO Bool
refreshIsSettled commit = Set.member commit . refreshSettled <$> readMVar refreshStateRef

scheduleCertificateRefresh :: Text -> IO ()
scheduleCertificateRefresh commit =
    modifyMVar_ refreshStateRef $ \state ->
        if refreshRunning state
            then pure state{refreshPending = Just commit}
            else do
                void $ forkIO $ refreshWorker commit
                pure state{refreshRunning = True, refreshPending = Nothing}

refreshWorker :: Text -> IO ()
refreshWorker commit = do
    Exception.catch (runAppEffects (runCertificateRefresh commit)) failure
    markRefreshSettled refreshStateRef commit
    next <- modifyMVar refreshStateRef $ \state ->
        case refreshPending state of
            Just pending -> pure (state{refreshPending = Nothing}, Just pending)
            Nothing -> pure (state{refreshRunning = False}, Nothing)
    maybe (pure ()) refreshWorker next
  where
    failure (err :: SomeException) = putStrLn $ "Certificate refresh failed: " ++ show err

runCertificateRefresh :: App es => Text -> Eff es ()
runCertificateRefresh commit = do
    declared <- certificateKeysAt commit
    case declared of
        Left err -> liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " skipped: " ++ err
        Right ids
            | versionedSchema ids < schemaVersionWithKeys ->
                liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " skipped: the revision publishes no step keys."
            | otherwise -> do
                keys <- certificateKeysFor commit (versionedValue ids)
                case keys of
                    Left err -> liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " failed: " ++ err
                    Right byStep -> refreshMissing commit byStep

refreshMissing :: App es => Text -> Map Int (Maybe Text) -> Eff es ()
refreshMissing commit keys = do
    let known = [(sid, key) | (sid, Just key) <- Map.toList keys]
    stored <- liftIO $ forM known $ \(sid, key) -> do
        path <- lookupCertificate key
        pure (sid, path)
    let missing = [sid | (sid, Nothing) <- stored]
    unless (null missing) $ do
        paths <- certificatePathsAt commit missing
        case paths of
            Left err -> liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " failed: " ++ err
            Right evaluated -> do
                let resolved = Map.mapMaybe id (Map.fromList evaluated)
                liftIO $ storeNew known (Map.toList resolved)
                broadcastRefreshed commit resolved

broadcastRefreshed :: App es => Text -> Map Int StepPaths -> Eff es ()
broadcastRefreshed commit certificates = do
    membership <- projectMembershipAt commit
    case membership of
        Left err -> liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " could not read projects: " ++ err
        Right projects -> do
            (statuses, _) <- rawStatusesFor (Map.map (pack . stepCertificate) certificates)
            marked <- markBuiltOutputs certificates statuses
            forM_ (Map.toList projects) $ \(pid, stepIds) -> do
                let targets = Set.intersection (Set.fromList stepIds) (Map.keysSet marked)
                when (not (Set.null targets)) $
                    liftIO $ broadcastSnapshot pid commit (Map.restrictKeys marked targets)

rawStatusesFor :: App es => Map Int Text -> Eff es (Map Int (Text, Maybe Text), StepStore)
rawStatusesFor certificates = do
    store <- buildStepStore certificates
    runningKeys <- runningBuildKeys
    statuses <- rawStatusesBatched store (isCertificateBuilding runningKeys) certificates
    return (statuses, store)

runningBuildKeys :: (Slurm :> es) => Eff es (Set.Set String)
runningBuildKeys = do
    jobs <- querySlurmJobs
    return $ Set.fromList (map slurmJobName jobs)

isCertificateBuilding :: Set.Set String -> Text -> Bool
isCertificateBuilding runningKeys certificate =
    Set.member (unBuildKey (buildKeyForOutPath (unpack certificate))) runningKeys

certificateKeysAt :: (Nix :> es, IOE :> es) => Text -> Eff es (Either String (Versioned [Int]))
certificateKeysAt commit = do
    liftIO $ ensureCommitForRefresh commit
    nixEvalJson commit "pointy" stepIdsExpression

certificateKeysFor :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String (Map Int (Maybe Text)))
certificateKeysFor commit = go []
  where
    go acc [] = pure $ Right (Map.unions (reverse acc))
    go acc pending = do
        let (batch, rest) = splitAt certificateBatchSize pending
        keys <- certificateKeysBatch commit batch
        case keys of
            Left err -> pure $ Left err
            Right resolved -> go (resolved : acc) rest
certificateKeysBatch :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String (Map Int (Maybe Text)))
certificateKeysBatch _ [] = pure $ Right Map.empty
certificateKeysBatch commit ids = do
    result <- nixEvalJson commit "pointy" (stepKeysExpression ids)
    case result of
        Right keys
            | Map.size (versionedValue keys) == length ids -> pure $ Right (versionedValue keys)
        failure -> splitOrReportKeysAt commit failure ids

splitOrReportKeysAt :: (Nix :> es, IOE :> es) => Text -> Either String (Versioned (Map Int (Maybe Text))) -> [Int] -> Eff es (Either String (Map Int (Maybe Text)))
splitOrReportKeysAt commit failure ids = case ids of
    [single] -> do
        liftIO $ putStrLn $ "Step " ++ show single ++ " has no key: " ++ describe
        pure $ Right (Map.singleton single Nothing)
    _ -> do
        let (left, right) = splitAt (length ids `div` 2) ids
        leftResult <- certificateKeysBatch commit left
        rightResult <- certificateKeysBatch commit right
        pure $ Map.union <$> leftResult <*> rightResult
  where
    describe = case failure of
        Left err -> err
        Right _ -> "the evaluation returned the wrong number of keys"

certificatePathsAt :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String [(Int, Maybe StepPaths)])
certificatePathsAt commit = go []
  where
    go acc [] = pure $ Right (reverse acc)
    go acc pending = do
        let (batch, rest) = splitAt certificateBatchSize pending
        paths <- certificatePathsFor commit batch
        case paths of
            Left err -> pure $ Left err
            Right evaluated -> go (reverse evaluated ++ acc) rest

certificatePathsFor :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String [(Int, Maybe StepPaths)])
certificatePathsFor _ [] = pure $ Right []
certificatePathsFor commit ids = do
    result <- nixEvalJson commit "pointy.steps" (stepCertificatesExpression ids)
    case result of
        Right (paths :: [Maybe StepPaths])
            | length paths == length ids -> pure $ Right (zip ids paths)
        failure -> splitOrReportPaths commit failure ids

splitOrReportPaths :: (Nix :> es, IOE :> es) => Text -> Either String [Maybe StepPaths] -> [Int] -> Eff es (Either String [(Int, Maybe StepPaths)])
splitOrReportPaths commit failure ids = case ids of
    [single] -> do
        liftIO $ putStrLn $ "Step " ++ show single ++ " has no certificate: " ++ describe
        pure $ Right [(single, Nothing)]
    _ -> do
        let (left, right) = splitAt (length ids `div` 2) ids
        leftResult <- certificatePathsFor commit left
        rightResult <- certificatePathsFor commit right
        pure $ (++) <$> leftResult <*> rightResult
  where
    describe = case failure of
        Left err -> err
        Right _ -> "the evaluation returned the wrong number of certificates"

projectMembershipAt :: App es => Text -> Eff es (Either String (Map Int [Int]))
projectMembershipAt commit = do
    result <- nixEvalJson commit "pointy.projects" projectMembershipExpression
    case result of
        Right (declared :: Map String [Int]) ->
            pure $ Right $ Map.fromList [(pid, sids) | (key, sids) <- Map.toList declared, Just pid <- [readInt key]]
        Left _ -> do
            repoPath <- liftIO userRepoPath
            fmap (Map.fromList . map membership . Map.elems) <$> runExceptT (cachedProjectDefinitions (ReadRepoContext repoPath (unpack commit)))
  where
    membership project = (projectDefId project, projectDefStepIds project)

readInt :: String -> Maybe Int
readInt text = case reads text of
    [(value, "")] -> Just value
    _ -> Nothing

nixEvalJson :: (Nix :> es, IOE :> es, FromJSON a) => Text -> String -> String -> Eff es (Either String a)
nixEvalJson commit attr applyExpr = do
    repoPath <- liftIO userRepoPath
    let installable = installableAt repoPath commit
    (code, stdout, stderr) <- runNixCli (nixEvalArgs installable attr applyExpr)
    pure $ case code of
        ExitSuccess -> maybe (Left "Failed to parse the evaluation output") Right (decodeJson stdout)
        ExitFailure _ -> Left (errorSummary stderr)

installableAt :: FilePath -> Text -> String
installableAt repoPath commit = "git+file://" ++ repoPath ++ "?rev=" ++ unpack commit ++ "&allRefs=true"

nixEvalArgs :: String -> String -> String -> [String]
nixEvalArgs installable attr applyExpr =
    [ "eval"
    , "--extra-experimental-features"
    , "nix-command flakes"
    , "--json"
    , installable ++ "#" ++ attr
    , "--apply"
    , applyExpr
    ]

ensureCommitForRefresh :: Text -> IO ()
ensureCommitForRefresh commit =
    runExceptT (ensureRepoCommit (unpack commit))
        >>= either (\err -> putStrLn ("Certificate refresh could not prepare " ++ unpack commit ++ ": " ++ err)) pure

withWriteRepoTransaction :: (IOE :> es) => (WriteRepoContext -> ExceptT String (Eff es) a) -> Eff es (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    when (isRight result) $ liftIO $ void $ forkIO scheduleHeadRefresh
    pure result

scheduleHeadRefresh :: IO ()
scheduleHeadRefresh =
    withReadRepoTransactionIO (pure . pack . readCommitHash) >>= \case
        Left err -> putStrLn $ "Certificate refresh skipped: " ++ err
        Right commit -> scheduleCertificateRefresh commit

decodeJson :: (FromJSON a) => String -> Maybe a
decodeJson = decode . TLE.encodeUtf8 . TL.pack
