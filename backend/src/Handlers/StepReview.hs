{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics (Generic)
import Handlers.Projects (rewriteNixFile)
import Handlers.Statuses (checkStatus, forkBroadcastStatusForStepProjectsAtHead)
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseFile, responseLBS)
import OutPaths (withWriteRepoTransaction)
import Servant (Handler, NoContent (..), ServerError (..), Tagged (..), err400, err409, err500)
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

getProjectReviewHandler :: Int -> Maybe Text -> Handler (Map String StepReviewReport)
getProjectReviewHandler projectId commit = do
    result <- liftIO $ withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContext (readRepoPath context)) commit
        stepIds <- projectStepIds viewed projectId
        reviews <- (<> Map.fromList [(stepId, Nothing) | stepId <- stepIds]) <$> stepReviews context stepIds
        reviewedOutputs <- reviewedOutPaths (readRepoPath context) reviews
        comparisons <- stepComparisons viewed reviews reviewedOutputs
        statuses <- mapM (either (\err -> pure ("failure", Just (T.pack err))) (liftIO . checkStatus)) reviewedOutputs
        let stepReport stepId review = StepReviewReport review (Map.lookup stepId statuses) (Map.findWithDefault NoReview stepId comparisons)
        pure $ Map.mapKeys show $ Map.mapWithKey stepReport reviews
    orFail err500 result

reviewStepHandler :: Int -> Maybe Text -> ReviewRequest -> Handler Bool
reviewStepHandler stepId mCommit request = do
    let by = T.unwords (T.words (requestedBy request))
        comments = T.strip (requestedComments request)
    when (T.null by) $ throwError err400{errBody = "Name who reviewed the step."}
    repoPath <- liftIO userRepoPath
    result <- liftIO $ withWriteRepoTransaction $ \context -> do
        viewed <- commitContext repoPath (fromMaybe "HEAD" mCommit)
        let revision = T.pack (readCommitHash viewed)
            advance = do
                setReview context stepId (Just (Review revision by comments))
                commitAndPushChanges context $ "review step " ++ show stepId ++ " by " ++ T.unpack by
                pure False
        review <- stepReview context stepId
        reviewedOutputs <- reviewedOutPaths repoPath (Map.singleton stepId review)
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

removeReviewHandler :: Int -> Handler NoContent
removeReviewHandler stepId = do
    removed <- liftIO (withWriteRepoTransaction $ \context -> do
        reviewed <- any isJust <$> stepReviews context [stepId]
        when reviewed $ do
            setReview context stepId Nothing
            commitAndPushChanges context $ "remove review of step " ++ show stepId
        pure reviewed) >>= orFail err409
    when removed $ liftIO (forkBroadcastStatusForStepProjectsAtHead stepId)
    pure NoContent

reviewDiffHandler :: Int -> Maybe Text -> Tagged Handler Application
reviewDiffHandler stepId mCommit = Tagged $ \_ respond -> do
    prepared <- withReadRepoTransaction $ \context -> do
        viewed <- maybe (pure context) (commitContext (readRepoPath context)) mCommit
        review <- stepReview context stepId
        reviewed <- maybe (throwError "This step has no review, so there is nothing to compare it with.") (commitContext (readRepoPath context) . reviewedRevision) review
        (,) <$> stepOutPath reviewed stepId <*> stepOutPath viewed stepId
    report <- runExceptT $ liftEither prepared >>= uncurry (renderReport stepId)
    respond $ either failure success report
  where
    failure message = responseLBS status500 [("Content-Type", "text/plain; charset=utf-8")] (TLE.encodeUtf8 (TL.pack message))
    success path =
        responseFile status200
            [ ("Content-Type", "text/html; charset=utf-8")
            , ("Content-Security-Policy", "sandbox allow-same-origin; default-src 'none'; img-src data:; style-src 'unsafe-inline'")
            , ("X-Content-Type-Options", "nosniff")
            ] path Nothing

renderReport :: Int -> FilePath -> FilePath -> ExceptT String IO FilePath
renderReport stepId reviewed viewed = do
    hashes <- storeHashes [reviewed, viewed]
    when (Map.notMember reviewed hashes) $ throwError $ T.unpack (reviewedOutputUnbuiltDetail stepId reviewed)
    when (Map.notMember viewed hashes) $ throwError $ "The viewed output is not built (" ++ viewed ++ "). Build the step to compare it."
    when (Map.lookup reviewed hashes == Map.lookup viewed hashes) $ throwError "The reviewed and viewed outputs are identical."
    dir <- (</> ".local/state/pointy/diff-reports") <$> liftIO getHomeDirectory
    let reportPath = dir </> "step-" ++ show stepId ++ "-" ++ storeHash reviewed ++ "-" ++ storeHash viewed ++ ".html"
    cached <- liftIO $ createDirectoryIfMissing True dir >> doesFileExist reportPath
    unless cached $ ExceptT $ withTempDirectory dir "report-" $ \stagingDir -> runExceptT $ do
        let staging = stagingDir </> "report.html"
        (code, _, stderr) <- liftIO $ readProcessWithExitCode "diffoscope" (comparisonArgs staging) ""
        unless (code `elem` [ExitSuccess, ExitFailure 1]) $
            throwError ("Output comparison failed: " ++ take 300 (unwords (words stderr)))
        liftIO $ do
            TIO.readFile staging >>= TIO.writeFile staging . retitle
            renameFile staging reportPath
    pure reportPath
  where
    storeHash = take 32 . takeFileName
    retitle html =
        let (before, rest) = T.breakOn "<title>" html
            (_, closing) = T.breakOn "</title>" rest
         in before <> "<title>Step " <> T.pack (show stepId) <> " · reviewed vs viewed" <> closing
    comparisonArgs out =
        words "--jquery disable --no-progress --output-empty --timeout 120 --max-report-size 8388608"
            ++ ["--html", out, reviewed, viewed]

stepComparisons :: ReadRepoContext -> StepReviews -> StepOutPaths -> ExceptT String IO (Map Int ReviewComparison)
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

ensureStepUnreviewed :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO ()
ensureStepUnreviewed ctx stepId = do
    reviews <- stepReviews ctx [stepId]
    when (any isJust reviews) $ throwError "Reviewed steps cannot be edited. Remove the review first."

requireStepUnreviewed :: Int -> Handler ()
requireStepUnreviewed stepId = liftIO (withReadRepoTransaction (`ensureStepUnreviewed` stepId)) >>= orFail err409

projectStepIds :: ReadRepoContext -> Int -> ExceptT String IO [Int]
projectStepIds context projectId =
    decodeNix "Failed to decode project step IDs" =<< runNixEvalJsonApplyInRepo context expression "#pointy.projects"
  where
    expression = "projects: map (step: step.def.id) (projects." ++ show (show projectId) ++ ".steps or (throw \"Project " ++ show projectId ++ " does not exist.\"))"

stepReview :: (RepoContext ctx) => ctx -> Int -> ExceptT String IO (Maybe Review)
stepReview ctx stepId = stepReviews ctx [stepId] >>= maybe (throwError ("Step " ++ show stepId ++ " does not exist.")) pure . Map.lookup stepId

stepReviews :: (RepoContext ctx) => ctx -> [Int] -> ExceptT String IO StepReviews
stepReviews _ [] = pure Map.empty
stepReviews ctx stepIds = do
    looked <- decodeNix "Failed to decode step reviews" =<< runNixEvalJsonApplyInRepo ctx (mapStepNames reviewOfExistingStep stepIds) "#pointy.stepDefs"
    pure $ Map.mapMaybe listToMaybe $ Map.fromList $ zip stepIds (looked :: [[Maybe Review]])
  where
    reviewOfExistingStep =
        "if builtins.hasAttr name steps then [ (let step = steps.${name}; in if (step.reviewedRevision or null) != null then { inherit (step) reviewedRevision; reviewedBy = step.reviewedBy or \"\"; reviewComments = step.reviewComments or \"\"; } else null) ] else []"

reviewedOutPaths :: FilePath -> StepReviews -> ExceptT String IO StepOutPaths
reviewedOutPaths repoPath reviews = Map.unions <$> mapM revisionPaths (Map.toList grouped)
  where
    grouped = Map.fromListWith (++) [(reviewedRevision review, [stepId]) | (stepId, Just review) <- Map.toList reviews]
    revisionPaths (revision, stepIds) = do
        result <- liftIO $ runExceptT $ commitContext repoPath revision >>= (`stepOutPaths` stepIds)
        pure $ either (\err -> Map.fromList [(stepId, Left err) | stepId <- stepIds]) id result

stepOutPath :: ReadRepoContext -> Int -> ExceptT String IO FilePath
stepOutPath context stepId = stepOutPaths context [stepId] >>= either throwError pure . lookupOutPath stepId

lookupOutPath :: Int -> StepOutPaths -> Either String FilePath
lookupOutPath stepId = Map.findWithDefault (Left ("Step " ++ show stepId ++ " has no output path.")) stepId

stepOutPaths :: ReadRepoContext -> [Int] -> ExceptT String IO StepOutPaths
stepOutPaths context stepIds = do
    resolved <- decodeNix "Failed to decode step output paths" =<< runNixEvalJsonApplyInRepo context (mapStepNames outPathExpression stepIds) "#pointy.steps"
    pure $ Map.fromList $ zip stepIds $ map entry (resolved :: [Maybe Text])
  where
    outPathExpression = "let path = builtins.tryEval (builtins.unsafeDiscardStringContext (toString steps.${name}.outPath)); in if path.success then path.value else null"
    entry = maybe (Left ("The output path of a step could not be evaluated at " ++ readCommitHash context ++ ".")) (Right . T.unpack . T.strip)

mapStepNames :: String -> [Int] -> String
mapStepNames expression stepIds = "steps: map (name: " ++ expression ++ ") [ " ++ unwords [show (show stepId) | stepId <- stepIds] ++ " ]"

storeHashes :: [FilePath] -> ExceptT String IO (Map FilePath Text)
storeHashes [] = pure Map.empty
storeHashes paths = do
    infos <- decodeNix "Failed to decode Nix path information" =<< runNix (["--offline", "path-info", "--json"] ++ Set.toList (Set.fromList paths))
    pure $ Map.mapMaybe (fmap narHash) (infos :: Map FilePath (Maybe PathInfo))

setReview :: WriteRepoContext -> Int -> Maybe Review -> ExceptT String IO ()
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

decodeNix :: (FromJSON a) => String -> String -> ExceptT String IO a
decodeNix label output = liftEither $ either (Left . ((label ++ ": ") ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))

orFail :: ServerError -> Either String a -> Handler a
orFail status = either (\message -> throwError status{errBody = TLE.encodeUtf8 (TL.pack message)}) pure
