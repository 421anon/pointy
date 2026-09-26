{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.StepReview (
    ReviewRequest (..),
    StepReviewReport (..),
    ensureStepUnreviewed,
    getProjectReviewHandler,
    removeReviewHandler,
    requireStepUnreviewed,
    reviewDiffHandler,
    reviewStepHandler,
    stepReviews,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effectful (Eff, IOE, (:>))
import EffectRunner (runAppEffects)
import Effects (AppEffects, AppM, Eval, Nix)
import GHC.Generics (Generic)
import Handlers.Projects (rewriteNixFile)
import BuildStatus (checkStatus)
import Handlers.Statuses (forkBroadcastStatusForStepProjectsAtHead)
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseLBS)
import NixStore (resolveStorePath)
import Certificates (withWriteRepoTransaction)
import Servant (NoContent (..), ServerError (..), Tagged (..), err400, err409, err500)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcessWithExitCode)
import UserRepo (ReadRepoContext (..), RepoContext, WriteRepoContext (..), commitAndPushChanges, commitContext, runNix, runNixEvalJsonApplyInRepo, userRepoPath, withReadRepoTransaction)

data ReviewComparison
    = NoReview
    | SameOutPath
    | SameContent
    | DifferentContent
    | ViewedOutputUnbuilt
    | ReviewedOutputUnbuilt Text
    | Unresolvable Text

data Review = Review
    { reviewedRevision :: Text
    , reviewedBy :: Text
    , reviewComments :: Text
    }
    deriving (Generic, FromJSON)

data ReviewRequest = ReviewRequest
    { requestedBy :: Text
    , requestedComments :: Text
    }

instance FromJSON ReviewRequest where
    parseJSON = withObject "ReviewRequest" $ \fields -> ReviewRequest <$> fields .: "reviewedBy" <*> fields .:? "reviewComments" .!= ""

data StepReviewReport = StepReviewReport (Maybe Review) (Maybe (Text, Maybe Text)) ReviewComparison

instance ToJSON StepReviewReport where
    toJSON (StepReviewReport mReview mStatus comparison) =
        object
            [ "reviewedRevision" .= fmap reviewedRevision mReview
            , "reviewedBy" .= fmap reviewedBy mReview
            , "reviewComments" .= fmap reviewComments mReview
            , "reviewedStatus" .= fmap fst mStatus
            , "reviewedStatusError" .= (mStatus >>= snd)
            , "comparison" .= (comparisonText :: Text)
            , "comparisonDetail" .= detail
            ]
      where
        (comparisonText, detail) = case comparison of
            NoReview -> ("no-review", Nothing)
            SameOutPath -> ("same-out-path", Nothing)
            SameContent -> ("same-content", Nothing)
            DifferentContent -> ("different-content", Nothing)
            ViewedOutputUnbuilt -> ("viewed-output-unbuilt", Nothing)
            ReviewedOutputUnbuilt text -> ("reviewed-output-unbuilt", Just text)
            Unresolvable text -> ("unresolvable", Just text)

type StepReviews = Map Int (Maybe Review)

type StepOutPaths = Map Int (Either String FilePath)

newtype PathInfo = PathInfo {narHash :: Text} deriving (Generic, FromJSON)

getProjectReviewHandler :: Int -> Maybe Text -> AppM (Map String StepReviewReport)
getProjectReviewHandler projectId commit = do
    result <- lift $ withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContextEff (readRepoPath context)) commit
        stepIds <- projectStepIds viewed projectId
        reviews <- (<> Map.fromList [(stepId, Nothing) | stepId <- stepIds]) <$> stepReviews context stepIds
        reviewedOutputs <- reviewedPaths (readRepoPath context) stepOutPaths reviews
        comparisons <- stepComparisons viewed reviews reviewedOutputs
        reviewedCertificates <- reviewedPaths (readRepoPath context) stepCertificatesOrLegacyOutPaths reviews
        statuses <- mapM (either (\err -> pure ("failure", Just (T.pack err))) (lift . checkStatus)) reviewedCertificates
        let stepReport stepId review = StepReviewReport review (Map.lookup stepId statuses) (Map.findWithDefault NoReview stepId comparisons)
        pure $ Map.mapKeys show $ Map.mapWithKey stepReport reviews
    orFail err500 result

reviewStepHandler :: Int -> Maybe Text -> ReviewRequest -> AppM Bool
reviewStepHandler stepId mCommit request = do
    let by = T.unwords (T.words (requestedBy request))
        comments = T.strip (requestedComments request)
    when (T.null by) $ throwError err400{errBody = "Name who reviewed the step."}
    repoPath <- liftIO userRepoPath
    result <- lift $ withWriteRepoTransaction $ \context -> do
        viewed <- commitContextEff repoPath (fromMaybe "HEAD" mCommit)
        let revision = T.pack (readCommitHash viewed)
            advance = do
                setReview context stepId (Just (Review revision by comments))
                commitAndPushChanges context $ "review step " ++ show stepId ++ " by " ++ T.unpack by
                pure False
        review <- stepReview context stepId
        reviewedOutputs <- reviewedPaths repoPath stepOutPaths (Map.singleton stepId review)
        comparison <- Map.findWithDefault NoReview stepId <$> stepComparisons viewed (Map.singleton stepId review) reviewedOutputs
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

removeReviewHandler :: Int -> AppM NoContent
removeReviewHandler stepId = do
    removed <- lift (withWriteRepoTransaction $ \context -> do
        reviewed <- any isJust <$> stepReviews context [stepId]
        when reviewed $ do
            setReview context stepId Nothing
            commitAndPushChanges context $ "remove review of step " ++ show stepId
        pure reviewed) >>= orFail err409
    when removed $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure NoContent

prepareReviewDiff :: Int -> Maybe Text -> Eff AppEffects (Either String Text)
prepareReviewDiff stepId mCommit = do
    prepared <- withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContextEff (readRepoPath context)) mCommit
        review <- stepReview context stepId
        reviewed <- maybe (throwError "This step has no review, so there is nothing to compare it with.") (commitContextEff (readRepoPath context) . reviewedRevision) review
        (,) <$> stepOutPath reviewed stepId <*> stepOutPath viewed stepId
    runExceptT $ do
        (reviewed, viewed) <- liftEither prepared
        path <- uncurry (renderReport stepId) (reviewed, viewed)
        dressReport stepId reviewed viewed <$> liftIO (TIO.readFile path)

reviewDiffHandler :: Int -> Maybe Text -> Tagged AppM Application
reviewDiffHandler stepId mCommit = Tagged $ \_ respond -> do
    report <- runAppEffects (prepareReviewDiff stepId mCommit)
    respond $ either failure success report
  where
    failure message = responseLBS status500 [("Content-Type", "text/plain; charset=utf-8")] (TLE.encodeUtf8 (TL.pack message))
    success html =
        responseLBS
            status200
            [ ("Content-Type", "text/html; charset=utf-8")
            , ("Content-Security-Policy", "sandbox allow-same-origin; default-src 'none'; img-src data:; style-src 'unsafe-inline'")
            , ("X-Content-Type-Options", "nosniff")
            ]
            (TLE.encodeUtf8 (TL.fromStrict html))

renderReport :: (Nix :> es, IOE :> es) => Int -> FilePath -> FilePath -> ExceptT String (Eff es) FilePath
renderReport stepId reviewed viewed = do
    hashes <- storeHashes [reviewed, viewed]
    when (Map.notMember reviewed hashes) $ throwError $ T.unpack (reviewedOutputUnbuiltDetail stepId reviewed)
    when (Map.notMember viewed hashes) $ throwError $ "The viewed output is not built (" ++ viewed ++ "). Build the step to compare it."
    when (Map.lookup reviewed hashes == Map.lookup viewed hashes) $ throwError "The reviewed and viewed outputs are identical."
    dir <- (</> ".local/state/pointy/diff-reports") <$> liftIO getHomeDirectory
    let reportPath = dir </> "step-" ++ show stepId ++ "-" ++ storeHash reviewed ++ "-" ++ storeHash viewed ++ ".html"
    cached <- liftIO $ createDirectoryIfMissing True dir >> doesFileExist reportPath
    unless cached $ do
        reviewedSource <- liftIO $ resolveStorePath reviewed
        viewedSource <- liftIO $ resolveStorePath viewed
        let relabel = T.replace (T.pack viewedSource) (T.pack viewed) . T.replace (T.pack reviewedSource) (T.pack reviewed)
        ExceptT $ liftIO $ withTempDirectory dir "report-" $ \stagingDir -> runExceptT $ do
            let staging = stagingDir </> "report.html"
            (code, _, stderr) <- liftIO $ readProcessWithExitCode "diffoscope" (comparisonArgs staging reviewedSource viewedSource) ""
            unless (code `elem` [ExitSuccess, ExitFailure 1]) $
                throwError ("Output comparison failed: " ++ take 300 (unwords (words stderr)))
            liftIO $ TIO.writeFile staging . relabel =<< TIO.readFile staging
            liftIO $ renameFile staging reportPath
    pure reportPath
  where
    storeHash = take 32 . takeFileName
    comparisonArgs out reviewedSource viewedSource =
        words "--jquery disable --no-progress --output-empty --timeout 120 --max-report-size 8388608"
            ++ ["--html", out, reviewedSource, viewedSource]

dressReport :: Int -> FilePath -> FilePath -> Text -> Text
dressReport stepId reviewed viewed =
    injectStyles . source viewed "viewed" . source reviewed "reviewed" . retitle
  where
    retitle html =
        let (before, rest) = T.breakOn "<title>" html
            (_, closing) = T.breakOn "</title>" rest
         in before <> "<title>Step " <> T.pack (show stepId) <> " · reviewed vs viewed" <> closing
    source path name = T.replace ("class=\"source\">" <> T.pack path) ("class=\"source\">" <> name)
    injectStyles html =
        let (before, rest) = T.breakOn "</head>" html
         in before <> reportStyles <> rest

reportStyles :: Text
reportStyles =
    T.unlines
        [ "<style>"
        , "body.diffoscope {"
        , "  --bg-primary: #1e1e1e;"
        , "  --bg-secondary: #252526;"
        , "  --bg-elevated: #2d2d30;"
        , "  --bg-hover: #38383b;"
        , "  --text-primary: #e6e6e6;"
        , "  --text-secondary: #b3b3b3;"
        , "  --text-muted: #808080;"
        , "  --border-color: #5a5a5a;"
        , "  --line-color: rgba(90, 90, 90, 0.3);"
        , "  --link: #0969da;"
        , "  --state-danger: #e05252;"
        , "  --state-danger-outline: rgba(224, 82, 82, 0.4);"
        , "  --row-added: rgba(46, 160, 67, 0.16);"
        , "  --row-deleted: rgba(224, 82, 82, 0.16);"
        , "  --row-changed: rgba(226, 183, 20, 0.12);"
        , "  --mark-added: rgba(46, 160, 67, 0.32);"
        , "  --mark-deleted: rgba(224, 82, 82, 0.32);"
        , "  --font-size-sm: 12px;"
        , "  --radius-sm: 6px;"
        , "  --spacing-2xs: 2px;"
        , "  --spacing-xs: 4px;"
        , "  --spacing-sm: 8px;"
        , "  margin: 0;"
        , "  padding: var(--spacing-sm);"
        , "  background: var(--bg-secondary);"
        , "  color: var(--text-primary);"
        , "  font: 14px/1.5 system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;"
        , "}"
        , ""
        , "@media (prefers-color-scheme: light) {"
        , "  body.diffoscope {"
        , "    --bg-primary: #fafafa;"
        , "    --bg-secondary: #eaeaeb;"
        , "    --bg-elevated: #ffffff;"
        , "    --bg-hover: #dfdfdf;"
        , "    --text-primary: #1a1a1a;"
        , "    --text-secondary: #454545;"
        , "    --text-muted: #6e6e6e;"
        , "    --border-color: #c4c4c4;"
        , "    --line-color: rgba(5, 80, 174, 0.3);"
        , "    --link: #0550ae;"
        , "    --state-danger: #cf222e;"
        , "    --state-danger-outline: rgba(207, 34, 46, 0.12);"
        , "    --row-added: rgba(26, 127, 55, 0.12);"
        , "    --row-deleted: rgba(207, 34, 46, 0.12);"
        , "    --row-changed: rgba(154, 103, 0, 0.12);"
        , "    --mark-added: rgba(26, 127, 55, 0.22);"
        , "    --mark-deleted: rgba(207, 34, 46, 0.22);"
        , "  }"
        , "}"
        , ""
        , ".diffoscope .difference {"
        , "  border: 1px solid var(--border-color);"
        , "  border-radius: var(--radius-sm);"
        , "  background: none;"
        , "  padding: var(--spacing-sm);"
        , "  margin: 0 0 var(--spacing-sm);"
        , "}"
        , ""
        , ".diffoscope > .difference {"
        , "  border: 0;"
        , "  border-radius: 0;"
        , "  padding: 0;"
        , "  margin: 0;"
        , "}"
        , ""
        , ".diffoscope > .difference > .diffheader {"
        , "  background: var(--bg-elevated);"
        , "  border: 1px solid var(--border-color);"
        , "  border-radius: var(--radius-sm);"
        , "  padding: var(--spacing-xs) var(--spacing-sm);"
        , "  margin: 0 0 var(--spacing-xs);"
        , "}"
        , ""
        , ".diffoscope .diffheader {"
        , "  padding: 0 0 var(--spacing-xs);"
        , "  font-size: var(--font-size-sm);"
        , "  color: var(--text-secondary);"
        , "}"
        , ""
        , ".diffoscope .source {"
        , "  color: var(--text-primary);"
        , "  font-weight: 600;"
        , "}"
        , ""
        , ".diffoscope .diffsize {"
        , "  color: var(--text-muted);"
        , "  font-family: monospace;"
        , "  font-size: 10px;"
        , "}"
        , ""
        , ".diffoscope .anchor {"
        , "  color: var(--text-muted);"
        , "  text-decoration: none;"
        , "}"
        , ""
        , ".diffoscope a {"
        , "  color: var(--link);"
        , "}"
        , ""
        , ".diffoscope table.diff {"
        , "  border: 0;"
        , "  border-collapse: collapse;"
        , "  width: 100%;"
        , "  table-layout: fixed;"
        , "  font-family: 'Courier New', monospace;"
        , "  font-size: var(--font-size-sm);"
        , "  word-break: break-word;"
        , "}"
        , ""
        , ".diffoscope table.diff td {"
        , "  border: 0;"
        , "  padding: 0 var(--spacing-xs);"
        , "  vertical-align: top;"
        , "}"
        , ""
        , ".diffoscope .diffline {"
        , "  color: var(--text-muted);"
        , "  text-align: right;"
        , "  user-select: none;"
        , "}"
        , ""
        , ".diffoscope .diffunmodified td {"
        , "  background: none;"
        , "}"
        , ""
        , ".diffoscope .diffchanged td {"
        , "  background: var(--row-changed);"
        , "}"
        , ""
        , ".diffoscope .diffadded td {"
        , "  background: var(--row-added);"
        , "}"
        , ""
        , ".diffoscope .diffdeleted td {"
        , "  background: var(--row-deleted);"
        , "}"
        , ""
        , ".diffoscope .diffhunk td {"
        , "  background: var(--bg-elevated);"
        , "  color: var(--text-muted);"
        , "  font-size: 11px;"
        , "  padding: var(--spacing-2xs) var(--spacing-xs);"
        , "}"
        , ""
        , ".diffoscope table.diff tr:hover td {"
        , "  background: color-mix(in srgb, var(--text-primary) 8%, transparent);"
        , "}"
        , ""
        , ".diffoscope ins {"
        , "  background: var(--mark-added);"
        , "}"
        , ""
        , ".diffoscope del {"
        , "  background: var(--mark-deleted);"
        , "}"
        , ""
        , ".diffoscope .dp {"
        , "  color: var(--text-muted);"
        , "  opacity: 0.6;"
        , "}"
        , ""
        , ".diffoscope th {"
        , "  background: var(--bg-elevated);"
        , "  color: var(--text-secondary);"
        , "}"
        , ""
        , ".diffoscope .comment {"
        , "  color: var(--text-secondary);"
        , "  font-style: italic;"
        , "}"
        , ""
        , ".diffoscope .comment.multiline {"
        , "  font-style: normal;"
        , "  font-family: monospace;"
        , "  white-space: pre;"
        , "}"
        , ""
        , ".diffoscope .error {"
        , "  border: 1px solid var(--state-danger);"
        , "  border-radius: var(--radius-sm);"
        , "  background: var(--state-danger-outline);"
        , "  color: var(--text-primary);"
        , "  padding: var(--spacing-xs);"
        , "}"
        , ""
        , ".diffoscope table.diff tr.ondemand td,"
        , ".diffoscope div.ondemand-details {"
        , "  background: var(--bg-elevated);"
        , "  color: var(--text-secondary);"
        , "}"
        , ""
        , ".diffoscope table.diff tr.ondemand:hover td,"
        , ".diffoscope div.ondemand-details:hover {"
        , "  background: var(--bg-hover);"
        , "  cursor: pointer;"
        , "}"
        , ""
        , ".diffoscope .footer {"
        , "  color: var(--text-muted);"
        , "  font-size: 11px;"
        , "  padding-top: var(--spacing-sm);"
        , "}"
        , "</style>"
        ]

stepComparisons :: (Eval :> es, Nix :> es) => ReadRepoContext -> StepReviews -> StepOutPaths -> ExceptT String (Eff es) (Map Int ReviewComparison)
stepComparisons viewed revisions reviewedOutputs
    | Map.null reviewed = pure $ NoReview <$ revisions
    | otherwise = do
        viewedOutputs <- stepOutPaths viewed (Map.keys reviewed)
        hashes <- storeHashes $ rights (Map.elems viewedOutputs) ++ rights (Map.elems reviewedOutputs)
        pure $ Map.mapWithKey (comparisonFor viewedOutputs hashes) revisions
  where
    reviewed = Map.mapMaybe id revisions
    comparisonFor viewedOutputs hashes stepId = \case
        Nothing -> NoReview
        Just _ -> case (lookupOutPath stepId viewedOutputs, lookupOutPath stepId reviewedOutputs) of
            (Left err, _) -> Unresolvable (oneLine err)
            (_, Left err) -> Unresolvable (oneLine err)
            (Right viewedPath, Right reviewedPath)
                | Map.notMember reviewedPath hashes -> ReviewedOutputUnbuilt (reviewedOutputUnbuiltDetail stepId reviewedPath)
                | viewedPath == reviewedPath -> SameOutPath
                | Map.notMember viewedPath hashes -> ViewedOutputUnbuilt
                | Map.lookup viewedPath hashes == Map.lookup reviewedPath hashes -> SameContent
                | otherwise -> DifferentContent
    oneLine = T.take 500 . T.unwords . T.words . T.pack

reviewedOutputUnbuiltDetail :: Int -> FilePath -> Text
reviewedOutputUnbuiltDetail stepId outPath =
    T.pack $ "Step " ++ show stepId ++ ": the reviewed revision has no built output (" ++ outPath ++ "). Rebuild the reviewed revision or remove the review."

ensureStepUnreviewed :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) ()
ensureStepUnreviewed ctx stepId = do
    reviews <- stepReviews ctx [stepId]
    when (any isJust reviews) $ throwError "Reviewed steps cannot be edited. Remove the review first."

requireStepUnreviewed :: Int -> AppM ()
requireStepUnreviewed stepId = lift (withReadRepoTransaction (`ensureStepUnreviewed` stepId)) >>= orFail err409

projectStepIds :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) [Int]
projectStepIds context projectId =
    decodeNix "Failed to decode project step IDs" =<< runNixEvalJsonApplyInRepo context expression "#pointy.projects"
  where
    expression = "projects: map (step: step.def.id) (projects." ++ show (show projectId) ++ ".steps or (throw \"Project " ++ show projectId ++ " does not exist.\"))"

stepReview :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) (Maybe Review)
stepReview ctx stepId = stepReviews ctx [stepId] >>= maybe (throwError ("Step " ++ show stepId ++ " does not exist.")) pure . Map.lookup stepId

stepReviews :: (RepoContext ctx, Eval :> es) => ctx -> [Int] -> ExceptT String (Eff es) StepReviews
stepReviews _ [] = pure Map.empty
stepReviews ctx stepIds = do
    looked <- decodeNix "Failed to decode step reviews" =<< runNixEvalJsonApplyInRepo ctx (mapStepNames "steps" reviewOfExistingStep stepIds) "#pointy.steps"
    pure $ Map.mapMaybe listToMaybe $ Map.fromList $ zip stepIds (looked :: [[Maybe Review]])
  where
    reviewOfExistingStep =
        "if builtins.hasAttr name steps then [ (let step = steps.${name}.def; in if (step.reviewedRevision or null) != null then { inherit (step) reviewedRevision; reviewedBy = step.reviewedBy or \"\"; reviewComments = step.reviewComments or \"\"; } else null) ] else []"

reviewedPaths :: (IOE :> es, Eval :> es) => FilePath -> (ReadRepoContext -> [Int] -> ExceptT String (Eff es) StepOutPaths) -> StepReviews -> ExceptT String (Eff es) StepOutPaths
reviewedPaths repoPath resolve reviews = Map.unions <$> mapM revisionPaths (Map.toList grouped)
  where
    grouped = Map.fromListWith (++) [(reviewedRevision review, [stepId]) | (stepId, Just review) <- Map.toList reviews]
    revisionPaths (revision, stepIds) = do
        viewedEither <- liftIO $ runExceptT $ commitContext repoPath revision
        result <- case viewedEither of
            Left err -> pure (Left err)
            Right viewed -> lift $ runExceptT $ resolve viewed stepIds
        pure $ either (\err -> Map.fromList [(stepId, Left err) | stepId <- stepIds]) id result

stepCertificatesOrLegacyOutPaths :: (Eval :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) StepOutPaths
stepCertificatesOrLegacyOutPaths context stepIds = do
    resolved <- decodeNix "Failed to decode step certificates" =<< runNixEvalJsonApplyInRepo context (mapStepNames "steps" certificateExpression stepIds) "#pointy.steps"
    pure $ Map.fromList $ zip stepIds $ map entry (resolved :: [Maybe Text])
  where
    certificateExpression = "let path = builtins.tryEval (builtins.unsafeDiscardStringContext (toString (steps.${name}.certificate or steps.${name}).outPath)); in if path.success then path.value else null"
    entry = maybe (Left ("The certificate of a step could not be evaluated at " ++ readCommitHash context ++ ".")) (Right . T.unpack . T.strip)

stepOutPath :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) FilePath
stepOutPath context stepId = stepOutPaths context [stepId] >>= either throwError pure . lookupOutPath stepId

lookupOutPath :: Int -> StepOutPaths -> Either String FilePath
lookupOutPath stepId = Map.findWithDefault (Left ("Step " ++ show stepId ++ " has no output path.")) stepId

stepOutPaths :: (Eval :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) StepOutPaths
stepOutPaths context stepIds = do
    resolved <- decodeNix "Failed to decode step output paths" =<< runNixEvalJsonApplyInRepo context (mapStepNames "steps" outPathExpression stepIds) "#pointy.steps"
    pure $ Map.fromList $ zip stepIds $ map entry (resolved :: [Maybe Text])
  where
    outPathExpression = "let path = builtins.tryEval (builtins.unsafeDiscardStringContext (toString steps.${name}.outPath)); in if path.success then path.value else null"
    entry = maybe (Left ("The output path of a step could not be evaluated at " ++ readCommitHash context ++ ".")) (Right . T.unpack . T.strip)

mapStepNames :: String -> String -> [Int] -> String
mapStepNames subject expression stepIds = subject ++ ": map (name: " ++ expression ++ ") [ " ++ unwords [show (show stepId) | stepId <- stepIds] ++ " ]"

storeHashes :: (Nix :> es) => [FilePath] -> ExceptT String (Eff es) (Map FilePath Text)
storeHashes [] = pure Map.empty
storeHashes paths = do
    infos <- decodeNix "Failed to decode Nix path information" =<< runNix (["--offline", "path-info", "--json"] ++ Set.toList (Set.fromList paths))
    pure $ Map.mapMaybe (fmap narHash) (infos :: Map FilePath (Maybe PathInfo))

setReview :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> Maybe Review -> ExceptT String (Eff es) ()
setReview (WriteRepoContext worktreePath) stepId mReview =
    rewriteNixFile (worktreePath </> "steps" </> show stepId ++ ".nix") $ case mReview of
        Just (Review revision by comments) ->
            "orig // { reviewedRevision = " <> nixString revision <> "; reviewedBy = " <> nixString by <> "; reviewComments = " <> nixString comments <> "; }"
        Nothing -> "builtins.removeAttrs orig [ \"reviewedRevision\" \"reviewedBy\" \"reviewComments\" ]"

nixString :: Text -> Text
nixString text = "\"" <> T.concatMap escape text <> "\""
  where
    escape character = Map.findWithDefault (T.singleton character) character escapes
    escapes = Map.fromList [('\\', "\\\\"), ('"', "\\\""), ('$', "\\$"), ('\n', "\\n"), ('\r', "\\r"), ('\t', "\\t")]

decodeNix :: (FromJSON a) => String -> String -> ExceptT String (Eff es) a
decodeNix label output = liftEither $ either (Left . ((label ++ ": ") ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))

commitContextEff :: (IOE :> es) => FilePath -> Text -> ExceptT String (Eff es) ReadRepoContext
commitContextEff repoPath commit = ExceptT $ liftIO $ runExceptT $ commitContext repoPath commit

orFail :: ServerError -> Either String a -> AppM a
orFail status = either (\message -> throwError status{errBody = TLE.encodeUtf8 (TL.pack message)}) pure
