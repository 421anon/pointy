{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepValidation (
    ValidationOutcome,
    ensureStepUnvalidated,
    getProjectValidationHandler,
    requireStepUnvalidated,
    stepDiffReportHandler,
    stepPins,
    unvalidateStepHandler,
    validateStepHandler,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (..), eitherDecode, object, (.=))
import Data.Either (rights)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Projects (rewriteNixFile)
import Handlers.Statuses (forkBroadcastStatusForStepProjectsAtHead)
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseFile, responseLBS)
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), withWriteRepoTransaction)
import Servant (Handler, NoContent (..), ServerError (..), Tagged (..), err409, err500)
import qualified Servant
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import UserRepo (
    ReadRepoContext (..),
    RepoContext,
    WriteRepoContext (..),
    commitAndPushChanges,
    runGitIn,
    runNix,
    runNixEvalJsonApplyInRepo,
    runNixEvalJsonInRepo,
    userRepoPath,
    withReadRepoTransaction,
 )

data ValidationOutcome
    = Unvalidated
    | Current
    | Identical
    | Differ
    | Unbuilt
    | MissingBaseline Text
    | CheckFailed Text

instance ToJSON ValidationOutcome where
    toJSON outcome = object ["verdict" .= verdict, "message" .= message]
      where
        (verdict, message) = case outcome of
            Unvalidated -> ("unvalidated" :: Text, Nothing)
            Current -> ("current", Nothing)
            Identical -> ("identical", Nothing)
            Differ -> ("differ", Nothing)
            Unbuilt -> ("unbuilt", Nothing)
            MissingBaseline detail -> ("missing", Just detail)
            CheckFailed detail -> ("error", Just detail)

type StepPins = Map Int (Maybe Text)

type StepOutPaths = Map Int (Either String FilePath)

newtype PathInfo = PathInfo {narHash :: Text}
    deriving (Generic, FromJSON)

getProjectValidationHandler :: Int -> Maybe Text -> Handler (Map String ValidationOutcome)
getProjectValidationHandler projectId commit = do
    result <- liftIO $ withReadRepoTransaction $ \context -> do
        target <- maybe (pure context) (commitContext (readRepoPath context)) commit
        pins <- stepPins target =<< projectStepIds target projectId
        Map.mapKeys show <$> stepOutcomes target pins
    orFail err500 result

validateStepHandler :: Int -> Handler Bool
validateStepHandler stepId = do
    repoPath <- liftIO userRepoPath
    result <- liftIO $ withWriteRepoTransaction $ \context@(WriteRepoContext worktreePath) -> do
        (code, stdout, stderr) <- liftIO $ runGitIn worktreePath ["rev-parse", "HEAD"]
        unless (code == ExitSuccess) $ throwError ("Failed to resolve the current commit: " ++ stderr)
        let commit = T.strip (T.pack stdout)
            target = ReadRepoContext repoPath (T.unpack commit)
            advance = do
                setValidationCommitHash context stepId (Just commit)
                commitAndPushChanges context $ "validate step " ++ show stepId
                pure False
        pin <- stepPin target stepId
        outcome <- Map.findWithDefault Unvalidated stepId <$> stepOutcomes target (Map.singleton stepId pin)
        case outcome of
            Differ -> pure True
            Unvalidated -> do
                outPath <- stepOutPath target stepId
                hashes <- storeHashes [outPath]
                when (Map.notMember outPath hashes) $
                    throwError ("Build the step before validating it. Output not in the store: " ++ outPath)
                advance
            Unbuilt -> throwError "Build the current output before updating its validation."
            MissingBaseline detail -> throwError (T.unpack detail)
            CheckFailed detail -> throwError (T.unpack detail)
            _ -> advance
    differs <- orFail err409 result
    unless differs $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure differs

unvalidateStepHandler :: Int -> Handler NoContent
unvalidateStepHandler stepId = do
    result <- liftIO $ withWriteRepoTransaction $ \context -> do
        pinned <- any isJust <$> stepPins context [stepId]
        when pinned $ do
            setValidationCommitHash context stepId Nothing
            commitAndPushChanges context $ "unvalidate step " ++ show stepId
        pure pinned
    unvalidated <- orFail err409 result
    when unvalidated $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure NoContent

stepDiffReportHandler :: Int -> Tagged Handler Application
stepDiffReportHandler stepId = Tagged $ \_ respond -> do
    prepared <- withReadRepoTransaction $ \context -> do
        pin <- stepPin context stepId
        baseline <- maybe (throwError "This step is not validated, so there is no baseline to compare.") (commitContext (readRepoPath context)) pin
        (,) <$> stepOutPath baseline stepId <*> stepOutPath context stepId
    report <- runExceptT $ liftEither prepared >>= uncurry (renderReport stepId)
    respond $ either failure success report
  where
    failure message =
        responseLBS status500 [("Content-Type", "text/plain; charset=utf-8")] (TLE.encodeUtf8 (TL.pack message))
    success path =
        responseFile
            status200
            [ ("Content-Type", "text/html; charset=utf-8")
            , ("Content-Security-Policy", "sandbox; default-src 'none'; img-src data:; style-src 'unsafe-inline'")
            , ("X-Content-Type-Options", "nosniff")
            ]
            path
            Nothing

renderReport :: Int -> FilePath -> FilePath -> ExceptT String IO FilePath
renderReport stepId baseline current = do
    hashes <- storeHashes [baseline, current]
    case (Map.lookup baseline hashes, Map.lookup current hashes) of
        (Nothing, _) -> throwError $ T.unpack (missingBaseline stepId baseline)
        (_, Nothing) -> throwError $ "The current output is not built (" ++ current ++ "). Build the step to compare it."
        (baselineHash, currentHash) | baselineHash == currentHash -> throwError "The validated and current outputs are identical."
        _ -> pure ()
    home <- liftIO getHomeDirectory
    let dir = home </> ".local/state/pointy/diff-reports"
        reportPath = dir </> "step-" ++ show stepId ++ "-" ++ storeHash baseline ++ "-" ++ storeHash current ++ ".html"
        staging = reportPath ++ ".part"
    liftIO $ createDirectoryIfMissing True dir
    cached <- liftIO $ doesFileExist reportPath
    unless cached $ do
        (code, _, stderr) <- liftIO $ readProcessWithExitCode "diffoscope" (comparisonArgs staging) ""
        unless (code `elem` [ExitSuccess, inputsDiffer]) $
            throwError ("Output comparison failed: " ++ take 300 (unwords (words stderr)))
        liftIO $ do
            TIO.readFile staging >>= TIO.writeFile staging . retitle
            renameFile staging reportPath
    pure reportPath
  where
    storeHash = take 32 . takeFileName
    inputsDiffer = ExitFailure 1
    retitle html =
        let (before, rest) = T.breakOn "<title>" html
            (_, closing) = T.breakOn "</title>" rest
         in before <> "<title>Step " <> T.pack (show stepId) <> " · validated vs current" <> closing
    comparisonArgs out =
        words "--jquery disable --no-progress --exclude-directory-metadata yes --timeout 120 --max-report-size 8388608"
            ++ ["--html", out, baseline, current]

stepOutcomes :: ReadRepoContext -> StepPins -> ExceptT String IO (Map Int ValidationOutcome)
stepOutcomes context pins
    | Map.null pinned = pure $ Unvalidated <$ pins
    | otherwise = do
        currents <- stepOutPaths context (Map.keys pinned)
        baselines <- baselineOutPaths (readRepoPath context) pinned
        hashes <- storeHashes $ resolved currents ++ resolved baselines
        pure $ Map.mapWithKey (outcomeFor currents baselines hashes) pins
  where
    pinned = Map.mapMaybe id pins
    resolved = rights . Map.elems

outcomeFor :: StepOutPaths -> StepOutPaths -> Map FilePath Text -> Int -> Maybe Text -> ValidationOutcome
outcomeFor currents baselines hashes stepId = \case
    Nothing -> Unvalidated
    Just _ -> either (CheckFailed . oneLine) id $ do
        current <- lookupOutPath stepId currents
        baseline <- lookupOutPath stepId baselines
        pure $ case (Map.lookup baseline hashes, Map.lookup current hashes) of
            (Nothing, _) -> MissingBaseline (missingBaseline stepId baseline)
            _ | current == baseline -> Current
            (_, Nothing) -> Unbuilt
            (baselineHash, currentHash)
                | baselineHash == currentHash -> Identical
                | otherwise -> Differ
  where
    oneLine = T.take 500 . T.unwords . T.words . T.pack

lookupOutPath :: Int -> StepOutPaths -> Either String FilePath
lookupOutPath stepId = Map.findWithDefault (Left ("Step " ++ show stepId ++ " has no output path.")) stepId

missingBaseline :: Int -> FilePath -> Text
missingBaseline stepId outPath =
    T.pack $ "Step " ++ show stepId ++ ": validated output missing (" ++ outPath ++ "). Rebuild the validated revision or unvalidate."

ensureStepUnvalidated :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO ()
ensureStepUnvalidated ctx stepId = do
    pins <- stepPins ctx [stepId]
    when (any isJust pins) $
        throwError "Validated steps cannot be edited. Unvalidate this step first."

requireStepUnvalidated :: Int -> Handler ()
requireStepUnvalidated stepId =
    liftIO (withReadRepoTransaction (`ensureStepUnvalidated` stepId)) >>= orFail err409

projectStepIds :: ReadRepoContext -> Int -> ExceptT String IO [Int]
projectStepIds context projectId = do
    defs <- decodeNix "Failed to decode #pointy.projects" =<< runNixEvalJsonInRepo context "#pointy.projects"
    case find ((== projectId) . projectDefId) (Map.elems (defs :: Map String ProjectDef)) of
        Nothing -> throwError $ "Project " ++ show projectId ++ " does not exist."
        Just project -> pure $ map (stepDefId . stepRefDef) (projectDefSteps project)

commitContext :: FilePath -> Text -> ExceptT String IO ReadRepoContext
commitContext repoPath hash = do
    let commit = T.unpack hash
    (exitCode, _, _) <- liftIO $ runGitIn repoPath ["cat-file", "-e", "--", commit ++ "^{commit}"]
    when (exitCode /= ExitSuccess) $ throwError ("Commit " ++ commit ++ " is not in the local user repository.")
    pure $ ReadRepoContext repoPath commit

stepPin :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO (Maybe Text)
stepPin ctx stepId =
    stepPins ctx [stepId]
        >>= maybe (throwError ("Step " ++ show stepId ++ " does not exist.")) pure . Map.lookup stepId

stepPins :: (RepoContext ctx) => ctx -> [Int] -> ExceptT String IO StepPins
stepPins _ [] = pure Map.empty
stepPins ctx stepIds = do
    looked <- decodeNix "Failed to decode validation commit hashes" =<< runNixEvalJsonApplyInRepo ctx (mapStepNames pinOfExistingStep stepIds) "#pointy.stepDefs"
    pure $ Map.mapMaybe listToMaybe $ Map.fromList $ zip stepIds (looked :: [[Maybe Text]])
  where
    pinOfExistingStep = "if builtins.hasAttr name steps then [ (steps.${name}.validationCommitHash or null) ] else []"

baselineOutPaths :: FilePath -> Map Int Text -> ExceptT String IO StepOutPaths
baselineOutPaths repoPath pinned = Map.unions <$> mapM revisionPaths (Map.toList grouped)
  where
    grouped = Map.fromListWith (++) [(pin, [stepId]) | (stepId, pin) <- Map.toList pinned]
    revisionPaths (pin, stepIds) =
        liftIO (runExceptT (commitContext repoPath pin)) >>= \case
            Left err -> pure $ Map.fromList [(stepId, Left err) | stepId <- stepIds]
            Right context -> stepOutPaths context stepIds

stepOutPath :: ReadRepoContext -> Int -> ExceptT String IO FilePath
stepOutPath context stepId =
    stepOutPaths context [stepId] >>= liftEither . lookupOutPath stepId

stepOutPaths :: ReadRepoContext -> [Int] -> ExceptT String IO StepOutPaths
stepOutPaths context stepIds = do
    resolved <- decodeNix "Failed to decode step output paths" =<< runNixEvalJsonApplyInRepo context (mapStepNames outPathExpression stepIds) "#pointy.steps"
    pure $ Map.fromList $ zip stepIds $ map entry (resolved :: [Maybe Text])
  where
    outPathExpression =
        "let path = builtins.tryEval (builtins.unsafeDiscardStringContext (toString steps.${name}.outPath)); "
            ++ "in if path.success then path.value else null"
    entry = maybe (Left ("The output path of a step could not be evaluated at " ++ readCommitHash context ++ ".")) (Right . T.unpack . T.strip)

mapStepNames :: String -> [Int] -> String
mapStepNames expression stepIds =
    "steps: map (name: " ++ expression ++ ") [ " ++ unwords [show (show stepId) | stepId <- stepIds] ++ " ]"

storeHashes :: [FilePath] -> ExceptT String IO (Map FilePath Text)
storeHashes [] = pure Map.empty
storeHashes paths = do
    infos <- decodeNix "Failed to decode Nix path information" =<< runNix (["--offline", "path-info", "--json"] ++ paths)
    pure $ Map.mapMaybe (fmap narHash) (infos :: Map FilePath (Maybe PathInfo))

setValidationCommitHash :: WriteRepoContext -> Int -> Maybe Text -> ExceptT String IO ()
setValidationCommitHash (WriteRepoContext worktreePath) stepId pin =
    rewriteNixFile (worktreePath </> "steps" </> show stepId ++ ".nix") $ case pin of
        Just hash -> "orig // { validationCommitHash = \"" <> hash <> "\"; }"
        Nothing -> "builtins.removeAttrs orig [ \"validationCommitHash\" ]"

decodeNix :: (FromJSON a) => String -> String -> ExceptT String IO a
decodeNix label output =
    liftEither $ either (Left . ((label ++ ": ") ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))

orFail :: ServerError -> Either String a -> Handler a
orFail status = either (\message -> Servant.throwError status{errBody = TLE.encodeUtf8 (TL.pack message)}) pure
