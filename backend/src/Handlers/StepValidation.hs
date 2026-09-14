{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepValidation (
    StepValidationReport (..),
    ensureStepUnvalidated,
    getProjectValidationHandler,
    requireStepUnvalidated,
    stepDiffReportHandler,
    stepPins,
    unvalidateStepHandler,
    validateStepHandler,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON (..), eitherDecode, object, (.=))
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Projects (rewriteNixFile)
import Handlers.Statuses (checkStatus, forkBroadcastStatusForStepProjectsAtHead)
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseFile, responseLBS)
import OutPaths (withWriteRepoTransaction)
import Servant (Handler, NoContent (..), ServerError (..), Tagged (..), err409, err500)
import qualified Servant
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcessWithExitCode)
import UserRepo (
    ReadRepoContext (..),
    RepoContext,
    WriteRepoContext (..),
    commitAndPushChanges,
    commitContext,
    runGitIn,
    runNix,
    runNixEvalJsonApplyInRepo,
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

outcomeFields :: ValidationOutcome -> (Text, Maybe Text)
outcomeFields = \case
    Unvalidated -> ("unvalidated", Nothing)
    Current -> ("current", Nothing)
    Identical -> ("identical", Nothing)
    Differ -> ("differ", Nothing)
    Unbuilt -> ("unbuilt", Nothing)
    MissingBaseline detail -> ("missing", Just detail)
    CheckFailed detail -> ("error", Just detail)

{- | A step's validation outcome together with the revision its baseline pins
and the build state of that pinned output, so a validated step can be shown as
it was at the pinned revision even when the requested revision differs.
-}
data StepValidationReport = StepValidationReport
    { reportPin :: Maybe Text
    , reportStatus :: Maybe (Text, Maybe Text)
    , reportOutcome :: ValidationOutcome
    }

instance ToJSON StepValidationReport where
    toJSON (StepValidationReport pin mStatus outcome) =
        object
            [ "pin" .= pin
            , "status" .= fmap fst mStatus
            , "error" .= (mStatus >>= snd)
            , "verdict" .= verdict
            , "message" .= message
            ]
      where
        (verdict, message) = outcomeFields outcome

type StepPins = Map Int (Maybe Text)

type StepOutPaths = Map Int (Either String FilePath)

newtype PathInfo = PathInfo {narHash :: Text}
    deriving (Generic, FromJSON)

getProjectValidationHandler :: Int -> Maybe Text -> Handler (Map String StepValidationReport)
getProjectValidationHandler projectId commit = do
    result <- liftIO $ withReadRepoTransaction $ \context -> do
        target <- maybe (pure context) (commitContext (readRepoPath context)) commit
        -- The baseline lives in current repository state, not in the requested
        -- revision, so a revision validated now reads back as validated.
        stepIds <- projectStepIds target projectId
        livePins <- stepPins context stepIds
        let pins = Map.union livePins (Map.fromList [(stepId, Nothing) | stepId <- stepIds])
        baselines <- baselineOutPaths (readRepoPath context) (Map.mapMaybe id pins)
        outcomes <- stepOutcomes target pins baselines
        statuses <- baselineStatuses baselines
        pure $
            Map.mapKeys show $
                Map.mapWithKey
                    (\stepId outcome -> StepValidationReport (Map.findWithDefault Nothing stepId pins) (Map.lookup stepId statuses) outcome)
                    outcomes
    orFail err500 result

{- | Build state of each pinned baseline output, so a validated step's row can
show the pinned revision's status rather than the requested revision's.
-}
baselineStatuses :: StepOutPaths -> ExceptT String IO (Map Int (Text, Maybe Text))
baselineStatuses = mapM $ \case
    Right outPath -> liftIO (checkStatus outPath)
    Left err -> pure ("failure", Just (T.pack err))

validateStepHandler :: Int -> Maybe Text -> Handler Bool
validateStepHandler stepId mCommit = do
    repoPath <- liftIO userRepoPath
    result <- liftIO $ withWriteRepoTransaction $ \context@(WriteRepoContext worktreePath) -> do
        (code, stdout, stderr) <- liftIO $ runGitIn worktreePath ["rev-parse", "HEAD"]
        unless (code == ExitSuccess) $ throwError ("Failed to resolve the current commit: " ++ stderr)
        let headCommit = T.unpack (T.strip (T.pack stdout))
        target <- case mCommit of
            Nothing -> pure (ReadRepoContext repoPath headCommit)
            Just commit -> commitContext repoPath commit
        let revision = T.pack (readCommitHash target)
            advance = do
                setValidationCommitHash context stepId (Just revision)
                commitAndPushChanges context $ "pin step " ++ show stepId
                pure False
        -- Compare against the baseline in current repository state, so an
        -- explicit revision is checked against what the branch records now.
        pin <- stepPin context stepId
        let pins = Map.singleton stepId pin
        baselines <- baselineOutPaths repoPath (Map.mapMaybe id pins)
        outcome <- Map.findWithDefault Unvalidated stepId <$> stepOutcomes target pins baselines
        case outcome of
            Differ -> pure True
            Unvalidated -> do
                outPath <- stepOutPath target stepId
                hashes <- storeHashes [outPath]
                when (Map.notMember outPath hashes) $
                    throwError ("Build the step before pinning it. Output not in the store: " ++ outPath)
                advance
            Unbuilt -> throwError ("Build the output at " ++ T.unpack revision ++ " before pinning it.")
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
            commitAndPushChanges context $ "unpin step " ++ show stepId
        pure pinned
    unvalidated <- orFail err409 result
    when unvalidated $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure NoContent

stepDiffReportHandler :: Int -> Maybe Text -> Tagged Handler Application
stepDiffReportHandler stepId mCommit = Tagged $ \_ respond -> do
    prepared <- withReadRepoTransaction $ \context -> do
        target <- maybe (pure context) (commitContext (readRepoPath context)) mCommit
        pin <- stepPin context stepId
        baseline <- maybe (throwError "This step is not pinned, so there is no baseline to compare.") (commitContext (readRepoPath context)) pin
        (,) <$> stepOutPath baseline stepId <*> stepOutPath target stepId
    report <- runExceptT $ liftEither prepared >>= uncurry (renderReport stepId)
    respond $ either failure success report
  where
    failure message =
        responseLBS status500 [("Content-Type", "text/plain; charset=utf-8")] (TLE.encodeUtf8 (TL.pack message))
    success path =
        responseFile
            status200
            [ ("Content-Type", "text/html; charset=utf-8")
            , ("Content-Security-Policy", "sandbox allow-same-origin; default-src 'none'; img-src data:; style-src 'unsafe-inline'")
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
        (baselineHash, currentHash) | baselineHash == currentHash -> throwError "The pinned and current outputs are identical."
        _ -> pure ()
    home <- liftIO getHomeDirectory
    let dir = home </> ".local/state/pointy/diff-reports"
        reportPath = dir </> "step-" ++ show stepId ++ "-" ++ storeHash baseline ++ "-" ++ storeHash current ++ ".html"
    liftIO $ createDirectoryIfMissing True dir
    cached <- liftIO $ doesFileExist reportPath
    unless cached $ ExceptT $ withTempDirectory dir "report-" $ \stagingDir -> runExceptT $ do
        let staging = stagingDir </> "report.html"
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
         in before <> "<title>Step " <> T.pack (show stepId) <> " · pinned vs current" <> closing
    comparisonArgs out =
        words "--jquery disable --no-progress --output-empty --timeout 120 --max-report-size 8388608"
            ++ ["--html", out, baseline, current]

stepOutcomes :: ReadRepoContext -> StepPins -> StepOutPaths -> ExceptT String IO (Map Int ValidationOutcome)
stepOutcomes context pins baselines
    | Map.null pinned = pure $ Unvalidated <$ pins
    | otherwise = do
        currents <- stepOutPaths context (Map.keys pinned)
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
    T.pack $ "Step " ++ show stepId ++ ": the pinned revision has no built output (" ++ outPath ++ "). Rebuild the pinned revision or unpin."

ensureStepUnvalidated :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO ()
ensureStepUnvalidated ctx stepId = do
    pins <- stepPins ctx [stepId]
    when (any isJust pins) $
        throwError "Pinned steps cannot be edited. Unpin this step first."

requireStepUnvalidated :: Int -> Handler ()
requireStepUnvalidated stepId =
    liftIO (withReadRepoTransaction (`ensureStepUnvalidated` stepId)) >>= orFail err409

projectStepIds :: ReadRepoContext -> Int -> ExceptT String IO [Int]
projectStepIds context projectId =
    decodeNix "Failed to decode project step IDs"
        =<< runNixEvalJsonApplyInRepo
            context
            ("projects: map (step: step.def.id) (projects." ++ show (show projectId) ++ ".steps or (throw \"Project " ++ show projectId ++ " does not exist.\"))")
            "#pointy.projects"

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
    revisionPaths (pin, stepIds) = do
        result <- liftIO $ runExceptT $ commitContext repoPath pin >>= (`stepOutPaths` stepIds)
        pure $ either (\err -> Map.fromList [(stepId, Left err) | stepId <- stepIds]) id result

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
    infos <- decodeNix "Failed to decode Nix path information" =<< runNix (["--offline", "path-info", "--json"] ++ Set.toList (Set.fromList paths))
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
