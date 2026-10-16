{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ProjectReads (projectRollupHandler) where

import ApiTypes (DynamicJson (..))
import BuildLog (resolveStatusesBatched)
import BuildStatus (StepPaths (..), markBuiltOutputs, stepStatusNames)
import Certificates (evalProjectDefinitions, getProjectCertificates, getStepCertificates, projectSchemaVersion, rawStatusesFor, schemaVersionWithKeys)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift, SeqUnlift), (:>), withEffToIO)
import Effects (App, AppM, Eval)
import Handlers.Projects (readJsonAtCommitWithHead)
import Handlers.StepReview (reviewedPaths, reviewedStatuses, stepCertificatesOrLegacyOutPaths, stepOutPaths, stepReviews)
import ProjectTree (ChildRef (..), projectChildRefs)
import RollupCache (lookupRollupCache, withRollupCache)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..))

projectRollupHandler :: Int -> Maybe T.Text -> AppM DynamicJson
projectRollupHandler projectId commit =
    readJsonAtCommitWithHead commit $ \headContext ctx -> do
        let key = (readCommitHash ctx, readCommitHash headContext, projectId)
        cached <- liftIO $ lookupRollupCache key
        case cached of
            Just value -> pure value
            Nothing -> do
                result <-
                    lift $
                        withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
                            withRollupCache key (unlift (runExceptT (projectRollup headContext ctx projectId)))
                either throwError pure result

projectRollup :: App es => ReadRepoContext -> ReadRepoContext -> Int -> ExceptT String (Eff es) Value
projectRollup headContext ctx projectId = do
    basis <-
        ExceptT $
            withEffToIO SeqUnlift $ \unlift ->
                cachedSuccess rollupBases (readCommitHash ctx, readCommitHash headContext, projectId) (unlift (runExceptT (rollupBasis headContext ctx projectId)))
    statuses <- lift $ rollupStatuses basis
    let rows = [rollupRow statuses (basisCertificates basis) child (Map.findWithDefault Set.empty child (basisSubtrees basis)) | child <- basisChildren basis]
    pure $ object ["project_id" .= projectId, "children" .= rows]

data RollupBasis = RollupBasis
    { basisChildren :: [Int]
    , basisSubtrees :: Map Int (Set Int)
    , basisCertificates :: Map Int (Map Int StepPaths)
    , basisUnreviewed :: Map Int StepPaths
    , basisReviewedCertificates :: Map Int (Either String FilePath)
    , basisReviewedOutputs :: Map Int (Either String FilePath)
    }

{-# NOINLINE rollupBases #-}
rollupBases :: SingleFlightCache (String, String, Int) RollupBasis
rollupBases = unsafePerformIO (newSingleFlightCache 16)

rollupBasis :: App es => ReadRepoContext -> ReadRepoContext -> Int -> ExceptT String (Eff es) RollupBasis
rollupBasis headContext ctx projectId = do
    projects <- evalProjectDefinitions ctx
    let children = directProjectChildren projects projectId
        graph = projectChildMap projects
        projectSteps = projectStepIds projects
        subtrees = Map.fromList [(child, subtree graph child) | child <- children]
        needed = Set.unions (Map.elems subtrees)
    version <- lift $ projectSchemaVersion ctx
    certificates <-
        if version >= schemaVersionWithKeys
            then do
                let neededSteps = Set.unions [Map.findWithDefault Set.empty pid projectSteps | pid <- Set.toList needed]
                result <- lift $ getStepCertificates (Set.toList neededSteps) (T.pack (readCommitHash ctx))
                case result of
                    Right resolved -> pure (Map.fromList [(pid, Map.restrictKeys resolved (Map.findWithDefault Set.empty pid projectSteps)) | pid <- Set.toList needed])
                    Left _ -> projectCertificatesOf ctx needed
            else projectCertificatesOf ctx needed
    let allCertificates = Map.unions (Map.elems certificates)
    reviews <- stepReviews headContext (Map.keys allCertificates)
    reviewedOutputs <- reviewedPaths (readRepoPath headContext) stepOutPaths reviews
    reviewedCertificates <- reviewedPaths (readRepoPath headContext) stepCertificatesOrLegacyOutPaths reviews
    pure
        RollupBasis
            { basisChildren = children
            , basisSubtrees = subtrees
            , basisCertificates = certificates
            , basisUnreviewed = Map.withoutKeys allCertificates (Map.keysSet (Map.mapMaybe id reviews))
            , basisReviewedCertificates = reviewedCertificates
            , basisReviewedOutputs = reviewedOutputs
            }

rollupStatuses :: App es => RollupBasis -> Eff es (Map Int (Text, Maybe Text))
rollupStatuses basis = do
    let unreviewed = basisUnreviewed basis
    (rawStatuses, store) <- rawStatusesFor (Map.map (T.pack . stepCertificate) unreviewed)
    unreviewedStatuses <- markBuiltOutputs unreviewed =<< resolveStatusesBatched store rawStatuses
    reviewed <- reviewedStatuses (basisReviewedCertificates basis) (basisReviewedOutputs basis)
    pure (Map.union reviewed unreviewedStatuses)

projectCertificatesOf :: (Eval :> es, IOE :> es) => ReadRepoContext -> Set Int -> ExceptT String (Eff es) (Map Int (Map Int StepPaths))
projectCertificatesOf ctx needed = Map.fromList <$> mapM (\pid -> (,) pid <$> projectCertificates ctx pid) (Set.toList needed)

projectCertificates :: (Eval :> es, IOE :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) (Map Int StepPaths)
projectCertificates ctx projectId = do
    result <- lift $ getProjectCertificates projectId (T.pack (readCommitHash ctx))
    case result of
        Right certificates -> pure certificates
        Left _ -> pure Map.empty

rollupRow :: Map Int (Text, Maybe Text) -> Map Int (Map Int StepPaths) -> Int -> Set Int -> Value
rollupRow statuses certificates child subtrees =
    object
        [ "id" .= child
        , "steps" .= Map.size steps
        , "statuses" .= countStatuses statuses (Map.keysSet steps)
        ]
  where
    steps = Map.unions [Map.findWithDefault Map.empty projectId certificates | projectId <- Set.toList subtrees]

countStatuses :: Map Int (Text, Maybe Text) -> Set Int -> Value
countStatuses statuses stepIds =
    object [Key.fromText key .= Map.findWithDefault 0 key counts | key <- stepStatusNames]
  where
    counts = Map.fromListWith (+) [(status, 1 :: Int) | stepId <- Set.toList stepIds, Just (status, _) <- [Map.lookup stepId statuses]]

subtree :: Map Int [Int] -> Int -> Set Int
subtree graph root = go Set.empty [root]
  where
    go seen [] = seen
    go seen (node : rest)
        | Set.member node seen = go seen rest
        | otherwise = go (Set.insert node seen) (Map.findWithDefault [] node graph ++ rest)

projectChildMap :: Value -> Map Int [Int]
projectChildMap projects =
    Map.fromList [(projectId, [childId | ProjectChild childId <- projectChildRefs project]) | (projectId, project) <- projectObjects projects]

projectStepIds :: Value -> Map Int (Set Int)
projectStepIds projects =
    Map.fromList [(projectId, Set.fromList [stepId | StepChild stepId <- projectChildRefs project]) | (projectId, project) <- projectObjects projects]

directProjectChildren :: Value -> Int -> [Int]
directProjectChildren projects projectId = Map.findWithDefault [] projectId (projectChildMap projects)

projectObjects :: Value -> [(Int, Object)]
projectObjects (Object projects) =
    [ (projectId, project)
    | (name, Object project) <- KeyMap.toList projects
    , Just projectId <- [readMaybe (Key.toString name)]
    ]
projectObjects _ = []
