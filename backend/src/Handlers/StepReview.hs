{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepReview (
    StepReviewReport (..),
    ensureStepUnreviewed,
    getProjectReviewHandler,
    removeReviewHandler,
    requireStepUnreviewed,
    reviewDiffHandler,
    reviewStepHandler,
    stepReviewRevisions,
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

{- | How the viewed revision's output relates to the revision the review
records. Path equality is answered first: a review that already names the
viewed output has nothing to compare, whatever the bytes are.
-}
data ReviewComparison
    = NoReview
    | SameOutPath
    | SameContent
    | DifferentContent
    | ViewedOutputUnbuilt
    | ReviewedOutputUnbuilt Text
    | Unresolvable Text

comparisonFields :: ReviewComparison -> (Text, Maybe Text)
comparisonFields = \case
    NoReview -> ("no-review", Nothing)
    SameOutPath -> ("same-out-path", Nothing)
    SameContent -> ("same-content", Nothing)
    DifferentContent -> ("different-content", Nothing)
    ViewedOutputUnbuilt -> ("viewed-output-unbuilt", Nothing)
    ReviewedOutputUnbuilt detail -> ("reviewed-output-unbuilt", Just detail)
    Unresolvable detail -> ("unresolvable", Just detail)

{- | A step's review comparison together with the revision its review records
and the build state of that reviewed output, so a reviewed step can be shown as
it was at the reviewed revision even when the viewed revision differs.
-}
data StepReviewReport = StepReviewReport
    { reportReviewedRevision :: Maybe Text
    , reportReviewedStatus :: Maybe (Text, Maybe Text)
    , reportComparison :: ReviewComparison
    }

instance ToJSON StepReviewReport where
    toJSON (StepReviewReport reviewedRevision mStatus comparison) =
        object
            [ "reviewedRevision" .= reviewedRevision
            , "reviewedStatus" .= fmap fst mStatus
            , "reviewedStatusError" .= (mStatus >>= snd)
            , "comparison" .= comparisonText
            , "comparisonDetail" .= detail
            ]
      where
        (comparisonText, detail) = comparisonFields comparison

type StepReviewRevisions = Map Int (Maybe Text)

type StepOutPaths = Map Int (Either String FilePath)

newtype PathInfo = PathInfo {narHash :: Text}
    deriving (Generic, FromJSON)

getProjectReviewHandler :: Int -> Maybe Text -> Handler (Map String StepReviewReport)
getProjectReviewHandler projectId commit = do
    result <- liftIO $ withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContext (readRepoPath context)) commit
        -- The review lives in current repository state, not in the viewed
        -- revision, so a revision reviewed now reads back as reviewed.
        stepIds <- projectStepIds viewed projectId
        liveReviews <- stepReviewRevisions context stepIds
        let reviews = Map.union liveReviews (Map.fromList [(stepId, Nothing) | stepId <- stepIds])
        reviewedOutputs <- reviewedOutPaths (readRepoPath context) (Map.mapMaybe id reviews)
        comparisons <- stepComparisons viewed reviews reviewedOutputs
        statuses <- reviewedOutputStatuses reviewedOutputs
        pure $
            Map.mapKeys show $
                Map.mapWithKey
                    (\stepId comparison -> StepReviewReport (Map.findWithDefault Nothing stepId reviews) (Map.lookup stepId statuses) comparison)
                    comparisons
    orFail err500 result

{- | Build state of each reviewed output, so a reviewed step's row can show the
reviewed revision's status rather than the viewed revision's.
-}
reviewedOutputStatuses :: StepOutPaths -> ExceptT String IO (Map Int (Text, Maybe Text))
reviewedOutputStatuses = mapM $ \case
    Right outPath -> liftIO (checkStatus outPath)
    Left err -> pure ("failure", Just (T.pack err))

reviewStepHandler :: Int -> Maybe Text -> Handler Bool
reviewStepHandler stepId mCommit = do
    repoPath <- liftIO userRepoPath
    result <- liftIO $ withWriteRepoTransaction $ \context@(WriteRepoContext worktreePath) -> do
        (code, stdout, stderr) <- liftIO $ runGitIn worktreePath ["rev-parse", "HEAD"]
        unless (code == ExitSuccess) $ throwError ("Failed to resolve the current commit: " ++ stderr)
        let headCommit = T.unpack (T.strip (T.pack stdout))
        viewed <- case mCommit of
            Nothing -> pure (ReadRepoContext repoPath headCommit)
            Just commit -> commitContext repoPath commit
        let revision = T.pack (readCommitHash viewed)
            advance = do
                setReviewedRevision context stepId (Just revision)
                commitAndPushChanges context $ "review step " ++ show stepId
                pure False
        -- Compare against the review in current repository state, so an explicit
        -- revision is compared with what the branch records now.
        reviewedRevision <- stepReviewRevision context stepId
        let reviews = Map.singleton stepId reviewedRevision
        reviewedOutputs <- reviewedOutPaths repoPath (Map.mapMaybe id reviews)
        comparison <- Map.findWithDefault NoReview stepId <$> stepComparisons viewed reviews reviewedOutputs
        case comparison of
            DifferentContent -> pure True
            NoReview -> do
                outPath <- stepOutPath viewed stepId
                hashes <- storeHashes [outPath]
                when (Map.notMember outPath hashes) $
                    throwError ("Build the step before reviewing it. Output not in the store: " ++ outPath)
                advance
            ViewedOutputUnbuilt -> throwError ("Build the output at " ++ T.unpack revision ++ " before reviewing it.")
            ReviewedOutputUnbuilt detail -> throwError (T.unpack detail)
            Unresolvable detail -> throwError (T.unpack detail)
            _ -> advance
    differs <- orFail err409 result
    unless differs $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure differs

removeReviewHandler :: Int -> Handler NoContent
removeReviewHandler stepId = do
    result <- liftIO $ withWriteRepoTransaction $ \context -> do
        reviewed <- any isJust <$> stepReviewRevisions context [stepId]
        when reviewed $ do
            setReviewedRevision context stepId Nothing
            commitAndPushChanges context $ "remove review of step " ++ show stepId
        pure reviewed
    removed <- orFail err409 result
    when removed $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure NoContent

reviewDiffHandler :: Int -> Maybe Text -> Tagged Handler Application
reviewDiffHandler stepId mCommit = Tagged $ \_ respond -> do
    prepared <- withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContext (readRepoPath context)) mCommit
        reviewedRevision <- stepReviewRevision context stepId
        reviewed <- maybe (throwError "This step has no review, so there is nothing to compare it with.") (commitContext (readRepoPath context)) reviewedRevision
        (,) <$> stepOutPath reviewed stepId <*> stepOutPath viewed stepId
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
renderReport stepId reviewed viewed = do
    hashes <- storeHashes [reviewed, viewed]
    case (Map.lookup reviewed hashes, Map.lookup viewed hashes) of
        (Nothing, _) -> throwError $ T.unpack (reviewedOutputUnbuiltDetail stepId reviewed)
        (_, Nothing) -> throwError $ "The viewed output is not built (" ++ viewed ++ "). Build the step to compare it."
        (reviewedHash, viewedHash) | reviewedHash == viewedHash -> throwError "The reviewed and viewed outputs are identical."
        _ -> pure ()
    home <- liftIO getHomeDirectory
    let dir = home </> ".local/state/pointy/diff-reports"
        reportPath = dir </> "step-" ++ show stepId ++ "-" ++ storeHash reviewed ++ "-" ++ storeHash viewed ++ ".html"
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
         in before <> "<title>Step " <> T.pack (show stepId) <> " · reviewed vs viewed" <> closing
    comparisonArgs out =
        words "--jquery disable --no-progress --output-empty --timeout 120 --max-report-size 8388608"
            ++ ["--html", out, reviewed, viewed]

stepComparisons :: ReadRepoContext -> StepReviewRevisions -> StepOutPaths -> ExceptT String IO (Map Int ReviewComparison)
stepComparisons viewed revisions reviewedOutputs
    | Map.null reviewed = pure $ NoReview <$ revisions
    | otherwise = do
        viewedOutputs <- stepOutPaths viewed (Map.keys reviewed)
        hashes <- storeHashes $ resolved viewedOutputs ++ resolved reviewedOutputs
        pure $ Map.mapWithKey (comparisonFor viewedOutputs reviewedOutputs hashes) revisions
  where
    reviewed = Map.mapMaybe id revisions
    resolved = rights . Map.elems

comparisonFor :: StepOutPaths -> StepOutPaths -> Map FilePath Text -> Int -> Maybe Text -> ReviewComparison
comparisonFor viewedOutputs reviewedOutputs hashes stepId = \case
    Nothing -> NoReview
    Just _ -> either (Unresolvable . oneLine) id $ do
        viewed <- lookupOutPath stepId viewedOutputs
        reviewed <- lookupOutPath stepId reviewedOutputs
        pure $ case (Map.lookup reviewed hashes, Map.lookup viewed hashes) of
            (Nothing, _) -> ReviewedOutputUnbuilt (reviewedOutputUnbuiltDetail stepId reviewed)
            _ | viewed == reviewed -> SameOutPath
            (_, Nothing) -> ViewedOutputUnbuilt
            (reviewedHash, viewedHash)
                | reviewedHash == viewedHash -> SameContent
                | otherwise -> DifferentContent
  where
    oneLine = T.take 500 . T.unwords . T.words . T.pack

lookupOutPath :: Int -> StepOutPaths -> Either String FilePath
lookupOutPath stepId = Map.findWithDefault (Left ("Step " ++ show stepId ++ " has no output path.")) stepId

reviewedOutputUnbuiltDetail :: Int -> FilePath -> Text
reviewedOutputUnbuiltDetail stepId outPath =
    T.pack $ "Step " ++ show stepId ++ ": the reviewed revision has no built output (" ++ outPath ++ "). Rebuild the reviewed revision or remove the review."

ensureStepUnreviewed :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO ()
ensureStepUnreviewed ctx stepId = do
    reviews <- stepReviewRevisions ctx [stepId]
    when (any isJust reviews) $
        throwError "Reviewed steps cannot be edited. Remove the review first."

requireStepUnreviewed :: Int -> Handler ()
requireStepUnreviewed stepId =
    liftIO (withReadRepoTransaction (`ensureStepUnreviewed` stepId)) >>= orFail err409

projectStepIds :: ReadRepoContext -> Int -> ExceptT String IO [Int]
projectStepIds context projectId =
    decodeNix "Failed to decode project step IDs"
        =<< runNixEvalJsonApplyInRepo
            context
            ("projects: map (step: step.def.id) (projects." ++ show (show projectId) ++ ".steps or (throw \"Project " ++ show projectId ++ " does not exist.\"))")
            "#pointy.projects"

stepReviewRevision :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO (Maybe Text)
stepReviewRevision ctx stepId =
    stepReviewRevisions ctx [stepId]
        >>= maybe (throwError ("Step " ++ show stepId ++ " does not exist.")) pure . Map.lookup stepId

stepReviewRevisions :: (RepoContext ctx) => ctx -> [Int] -> ExceptT String IO StepReviewRevisions
stepReviewRevisions _ [] = pure Map.empty
stepReviewRevisions ctx stepIds = do
    looked <- decodeNix "Failed to decode reviewed revisions" =<< runNixEvalJsonApplyInRepo ctx (mapStepNames reviewedRevisionOfExistingStep stepIds) "#pointy.stepDefs"
    pure $ Map.mapMaybe listToMaybe $ Map.fromList $ zip stepIds (looked :: [[Maybe Text]])
  where
    reviewedRevisionOfExistingStep = "if builtins.hasAttr name steps then [ (steps.${name}.reviewedRevision or null) ] else []"

reviewedOutPaths :: FilePath -> Map Int Text -> ExceptT String IO StepOutPaths
reviewedOutPaths repoPath reviewed = Map.unions <$> mapM revisionPaths (Map.toList grouped)
  where
    grouped = Map.fromListWith (++) [(revision, [stepId]) | (stepId, revision) <- Map.toList reviewed]
    revisionPaths (revision, stepIds) = do
        result <- liftIO $ runExceptT $ commitContext repoPath revision >>= (`stepOutPaths` stepIds)
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

setReviewedRevision :: WriteRepoContext -> Int -> Maybe Text -> ExceptT String IO ()
setReviewedRevision (WriteRepoContext worktreePath) stepId revision =
    rewriteNixFile (worktreePath </> "steps" </> show stepId ++ ".nix") $ case revision of
        Just hash -> "orig // { reviewedRevision = \"" <> hash <> "\"; }"
        Nothing -> "builtins.removeAttrs orig [ \"reviewedRevision\" ]"

decodeNix :: (FromJSON a) => String -> String -> ExceptT String IO a
decodeNix label output =
    liftEither $ either (Left . ((label ++ ": ") ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))

orFail :: ServerError -> Either String a -> Handler a
orFail status = either (\message -> Servant.throwError status{errBody = TLE.encodeUtf8 (TL.pack message)}) pure
