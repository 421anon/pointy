{-# LANGUAGE DeriveGeneric #-}
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
    decodeProjectDefinitions,
    withWriteRepoTransaction,
    scheduleCertificateRefresh,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import BuildStatus (checkStatus, resolveStepStatus)
import Bus (broadcastSnapshot)
import CertificateStore (lookupCertificate, storeCertificate)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, eitherDecode, genericParseJSON, withObject, (.:))
import Data.Char (toLower)
import Data.Either (isRight)
import Data.List (isPrefixOf, stripPrefix)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (catch)
import Effects (App, Eval, Nix, Slurm, runNixCli)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), RepoContext, WriteRepoContext, ensureRepoCommit, runNixEvalJsonApplyInRepo, userRepoPath, withReadRepoTransactionIO, withWriteRepoTransactionRaw)

data ProjectDef = ProjectDef
    { projectDefId :: Int
    , projectDefSteps :: [StepRef]
    }
    deriving (Show, Generic)

instance FromJSON ProjectDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "projectDef"

newtype StepRef = StepRef
    { stepRefDef :: StepDef
    }
    deriving (Show, Generic)

instance FromJSON StepRef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepRef"

newtype StepDef = StepDef
    { stepDefId :: Int
    }
    deriving (Show, Generic)

instance FromJSON StepDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepDef"

prefixedFieldOptions :: String -> Options
prefixedFieldOptions prefix =
    defaultOptions
        { fieldLabelModifier = \field ->
            map toLower (fromMaybe field (stripPrefix prefix field))
        }

invalidCertificate :: Text
invalidCertificate = "/invalid"

schemaVersionWithKeys :: Int
schemaVersionWithKeys = 1

data Versioned a = Versioned
    { versionedSchema :: Int
    , versionedValue :: a
    }

instance (FromJSON a) => FromJSON (Versioned a) where
    parseJSON = withObject "Versioned" $ \object ->
        Versioned <$> object .: "version" <*> object .: "value"

getProjectCertificates :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Map Int Text))
getProjectCertificates pid targetCommit = runExceptT $ do
    ctx <- prepareCommit targetCommit
    declared <-
        withExceptT ("Failed to evaluate project step ids: " ++) $
            runJson ctx "#pointy" (projectStepIdsExpression pid)
    let ids = versionedValue declared
    paths <-
        if versionedSchema declared >= schemaVersionWithKeys
            then do
                keys <- resolveKeys ctx ids
                let known = [(sid, key) | (sid, Just key) <- Map.toList keys]
                liftIO $ scheduleRefreshIfMissing targetCommit (map snd known)
                resolveCertificates ctx known
            else
                withExceptT ("Failed to evaluate project certificates: " ++) $
                    fmap (Map.mapMaybe (fmap unpack)) $
                        runJson ctx (projectAttr pid) legacyProjectCertificates
    pure $
        Map.union (Map.map pack paths) $
            Map.fromList [(sid, invalidCertificate) | sid <- ids, Map.notMember sid paths]

legacyProjectCertificates :: String
legacyProjectCertificates = "project: project.certificates or project.outPaths"

resolveKeys :: (Eval :> es, IOE :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) (Map Int (Maybe Text))
resolveKeys _ [] = pure Map.empty
resolveKeys ctx ids = do
    outcome <- lift $ runExceptT $ runNixEvalJsonApplyInRepo ctx (stepKeysExpression ids) "#pointy"
    case outcome of
        Right output -> case decodeJson output of
            Just keys
                | Map.size (versionedValue keys) == length ids -> pure (versionedValue keys)
            _ -> throwError "Failed to parse the step key evaluation"
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
    stored <- mapM lookupCertificate keys
    when (any isNothing stored) $ scheduleCertificateRefresh commit

getStepCertificate :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Maybe Text))
getStepCertificate sid targetCommit = runExceptT $ do
    ctx <- prepareCommit targetCommit
    resolved <-
        withExceptT ("Failed to evaluate step key: " ++) $
            runJson ctx "#pointy" (stepKeyExpression sid)
    case versionedValue resolved of
        Just key | versionedSchema resolved >= schemaVersionWithKeys ->
            fmap pack <$> Map.lookup sid <$> resolveCertificates ctx [(sid, key)]
        _ -> do
            evaluated <- resolveCertificatePaths ctx [sid]
            pure $ case lookup sid evaluated of
                Just (Just path) -> Just path
                _ -> Nothing

resolveCertificates :: (Eval :> es, IOE :> es) => ReadRepoContext -> [(Int, Text)] -> ExceptT String (Eff es) (Map Int FilePath)
resolveCertificates _ [] = pure Map.empty
resolveCertificates ctx keys = do
    stored <- liftIO $ forM keys $ \(sid, key) -> do
        path <- lookupCertificate key
        pure (sid, path)
    let fromStore = Map.fromList [(sid, path) | (sid, Just path) <- stored]
        missing = [sid | (sid, Nothing) <- stored]
    if null missing
        then pure fromStore
        else do
            evaluated <- resolveCertificatePaths ctx missing
            let evaluatedPaths = Map.mapMaybe (fmap unpack) $ Map.fromList evaluated
            liftIO $ storeNew keys (Map.toList evaluatedPaths)
            pure $ Map.union evaluatedPaths fromStore

resolveCertificatePaths :: (Eval :> es, IOE :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) [(Int, Maybe Text)]
resolveCertificatePaths _ [] = pure []
resolveCertificatePaths ctx ids = do
    outcome <- lift $ runExceptT $ runNixEvalJsonApplyInRepo ctx (stepCertificatesExpression ids) "#pointy.steps"
    case outcome of
        Right output -> case decodeJson output of
            Just paths
                | length paths == length ids -> pure $ zip ids paths
            _ -> throwError "Failed to parse the certificate evaluation"
        Left err -> splitOrReport ctx err ids

splitOrReport :: (Eval :> es, IOE :> es) => ReadRepoContext -> String -> [Int] -> ExceptT String (Eff es) [(Int, Maybe Text)]
splitOrReport ctx err ids = case ids of
    [single] -> do
        liftIO $ putStrLn $ "Step " ++ show single ++ " has no certificate: " ++ errorSummary err
        pure [(single, Nothing)]
    _ -> do
        let (left, right) = splitAt (length ids `div` 2) ids
        (++) <$> resolveCertificatePaths ctx left <*> resolveCertificatePaths ctx right

errorSummary :: String -> String
errorSummary text = case filter isErrorLine (lines text) of
    [] -> firstLine text
    linesWithError -> last linesWithError
  where
    isErrorLine line = "error:" `isPrefixOf` dropWhile (== ' ') line

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

storeNew :: [(Int, Text)] -> [(Int, FilePath)] -> IO ()
storeNew keys paths =
    forM_ paths $ \(sid, path) ->
        case lookup sid keys of
            Just key -> storeCertificate key path
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

runJson :: (Eval :> es, FromJSON a) => ReadRepoContext -> String -> String -> ExceptT String (Eff es) a
runJson ctx attr applyExpr = do
    output <- runNixEvalJsonApplyInRepo ctx applyExpr attr
    maybe (throwError $ "Failed to parse the evaluation of " ++ attr) pure (decodeJson output)

evalProjectDefinitions :: (RepoContext ctx, Eval :> es) => ctx -> ExceptT String (Eff es) String
evalProjectDefinitions ctx = runNixEvalJsonApplyInRepo ctx projectDefinitions projectsAttr

evalProjectDefinition :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) String
evalProjectDefinition ctx pid = runNixEvalJsonApplyInRepo ctx projectDefinition (projectAttr pid)

decodeProjectDefinitions :: String -> Either String (Map String ProjectDef)
decodeProjectDefinitions = either (Left . (("Failed to parse " ++ projectsAttr ++ ": ") ++)) Right . eitherDecode . TLE.encodeUtf8 . TL.pack

projectsAttr :: String
projectsAttr = "#pointy.projects"

projectAttr :: Int -> String
projectAttr pid = projectsAttr ++ "." ++ show pid

projectDefinitions :: String
projectDefinitions = "builtins.mapAttrs (_: " ++ projectDefinition ++ ")"

projectDefinition :: String
projectDefinition = "project: builtins.removeAttrs project [ \"outPaths\" \"certificates\" ]"

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
        ++ "{ inherit version; value = builtins.map (s: s.def.id) pointy.projects."
        ++ show (show pid)
        ++ ".steps; }"

schemaVersionPreamble :: String
schemaVersionPreamble = "let version = let t = builtins.tryEval (pointy.schemaVersion or 0); in if t.success then t.value else 0; in "

keyGuard :: String -> String
keyGuard selection =
    "if version >= "
        ++ show schemaVersionWithKeys
        ++ " then (let t = builtins.tryEval ("
        ++ selection
        ++ "); in if t.success then t.value else null) else null"

stepCertificatesExpression :: [Int] -> String
stepCertificatesExpression ids =
    "steps: map (id: let t = builtins.tryEval (builtins.unsafeDiscardStringContext (toString (steps.${id}.certificate or steps.${id}).outPath)); in if t.success then t.value else null) "
        ++ renderIdList ids

renderIdList :: [Int] -> String
renderIdList ids = "[ " ++ unwords (map (show . show) ids) ++ " ]"

projectMembershipExpression :: String
projectMembershipExpression = "projects: builtins.mapAttrs (_: p: builtins.map (s: s.def.id) p.steps) projects"

certificateBatchSize :: Int
certificateBatchSize = 512

data RefreshState = RefreshState
    { refreshRunning :: Bool
    , refreshPending :: Maybe Text
    }

{-# NOINLINE refreshStateRef #-}
refreshStateRef :: MVar RefreshState
refreshStateRef = unsafePerformIO (newMVar (RefreshState False Nothing))

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
                let resolved = Map.mapMaybe (fmap unpack) (Map.fromList evaluated)
                liftIO $ storeNew known (Map.toList resolved)
                broadcastRefreshed commit (Map.map pack resolved)

broadcastRefreshed :: App es => Text -> Map Int Text -> Eff es ()
broadcastRefreshed commit certificates = do
    membership <- projectMembershipAt commit
    case membership of
        Left err -> liftIO $ putStrLn $ "Certificate refresh for " ++ unpack commit ++ " could not read projects: " ++ err
        Right projects ->
            forM_ (Map.toList projects) $ \(pid, stepIds) -> do
                let targets = Set.toList (Set.intersection (Set.fromList stepIds) (Map.keysSet certificates))
                when (not (null targets)) $ do
                    statuses <- Map.fromList <$> mapM (stepStatus certificates) targets
                    liftIO $ broadcastSnapshot pid commit statuses

stepStatus :: (Nix :> es, Slurm :> es, IOE :> es) => Map Int Text -> Int -> Eff es (Int, (Text, Maybe Text))
stepStatus certificates sid = do
    let certificate = Map.lookup sid certificates
    raw <- case fmap unpack certificate of
        Just path -> checkStatus path `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
        Nothing -> pure ("not-started", Nothing)
    resolveStepStatus (fmap unpack certificate) (sid, raw)

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
        Left err -> errorSummary err
        Right _ -> "the evaluation returned the wrong number of keys"

certificatePathsAt :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String [(Int, Maybe Text)])
certificatePathsAt commit = go []
  where
    go acc [] = pure $ Right (reverse acc)
    go acc pending = do
        let (batch, rest) = splitAt certificateBatchSize pending
        paths <- certificatePathsFor commit batch
        case paths of
            Left err -> pure $ Left err
            Right evaluated -> go (reverse evaluated ++ acc) rest

certificatePathsFor :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Either String [(Int, Maybe Text)])
certificatePathsFor _ [] = pure $ Right []
certificatePathsFor commit ids = do
    result <- nixEvalJson commit "pointy.steps" (stepCertificatesExpression ids)
    case result of
        Right (paths :: [Maybe Text])
            | length paths == length ids -> pure $ Right (zip ids paths)
        failure -> splitOrReportPaths commit failure ids

splitOrReportPaths :: (Nix :> es, IOE :> es) => Text -> Either String [Maybe Text] -> [Int] -> Eff es (Either String [(Int, Maybe Text)])
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
        Left err -> errorSummary err
        Right _ -> "the evaluation returned the wrong number of certificates"

projectMembershipAt :: (Nix :> es, IOE :> es) => Text -> Eff es (Either String (Map Int [Int]))
projectMembershipAt commit = do
    result <- nixEvalJson commit "pointy.projects" projectMembershipExpression
    pure $ case result of
        Left err -> Left err
        Right (declared :: Map String [Int]) ->
            Right $ Map.fromList [(pid, sids) | (key, sids) <- Map.toList declared, Just pid <- [readInt key]]

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
        ExitFailure _ -> Left stderr

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
