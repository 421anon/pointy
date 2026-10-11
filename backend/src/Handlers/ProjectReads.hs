{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ProjectReads (unfiledHandler, projectRollupHandler) where

import ApiTypes (DynamicJson (..))
import BuildLog (resolveStatusesBatched)
import BuildStatus (StepPaths (..), markBuiltOutputs, stepStatusNames)
import Certificates (evalProjectDefinitions, getProjectCertificates, getStepCertificates, projectSchemaVersion, rawStatusesFor, schemaVersionWithKeys)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Object, Result (..), Value (..), eitherDecode, fromJSON, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Effectful (Eff, IOE, (:>))
import Effects (App, AppM, Eval)
import Handlers.Projects (annotateRecordChildren, readJsonAtCommit, readRecordMtimes)
import ProjectFiles (RawProjectFile (..), loadRawProjectFilesAt, loadUnfiledStepEntries)
import ProjectTree (ChildRef (..), normalizeProject, projectChildRefs)
import RollupCache (currentRollupGeneration, insertRollupCache, lookupRollupCache)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..))

unfiledHandler :: Maybe T.Text -> AppM DynamicJson
unfiledHandler commit =
    readJsonAtCommit commit $ \ctx -> do
        membership <- projectMembership ctx
        times <- liftIO $ readRecordMtimes (readRepoPath ctx) (readCommitHash ctx)
        steps <- loadUnfiledStepEntries ctx (Set.toList (linkedStepIds membership))
        pure (object ["children" .= annotateRecordChildren times (toJSON steps)])

projectMembership :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) (Map Int [ChildRef])
projectMembership ctx = do
    files <- loadRawProjectFilesAt ctx
    Map.fromList <$> mapM projectRefs files
  where
    projectRefs file = do
        let projectId = rawFileId file
            unreadable reason = throwError ("projects/" ++ show projectId ++ ".nix cannot be read: " ++ reason)
        payload <- maybe (unreadable "the file could not be evaluated") pure (rawFilePayload file)
        value <- either (unreadable . ("invalid JSON: " ++)) pure (eitherDecode (TLE.encodeUtf8 (TL.fromStrict payload)))
        case value of
            Object fields -> do
                current <- either unreadable pure (currentFields fields)
                case KeyMap.lookup "children" current of
                    Just (Array items) -> either unreadable (\refs -> pure (projectId, refs)) (traverse childRefOf (V.toList items))
                    _ -> unreadable "the file has no children attribute"
            _ -> unreadable "the project file is not an attribute set"
    currentFields fields = case KeyMap.lookup "children" fields of
        Just (Array _) -> Right fields
        Just _ -> Left "the children attribute is not a list"
        Nothing -> case KeyMap.lookup "steps" fields of
            Just (Array _) -> case normalizeProject (Object fields) of
                Object normalized -> Right normalized
                _ -> Left "the project file is not an attribute set"
            Just _ -> Left "the steps attribute is not a list"
            Nothing -> Left "the file has no children attribute"
    childRefOf item = case fromJSON item of
        Success ref -> Right ref
        Error err -> Left err

linkedStepIds :: Map Int [ChildRef] -> Set Int
linkedStepIds membership = Set.fromList [stepId | refs <- Map.elems membership, StepChild stepId <- refs]

projectRollupHandler :: Int -> Maybe T.Text -> AppM DynamicJson
projectRollupHandler projectId commit =
    readJsonAtCommit commit $ \ctx -> do
        let targetCommit = readCommitHash ctx
        cached <- liftIO $ lookupRollupCache targetCommit projectId
        case cached of
            Just value -> pure value
            Nothing -> do
                generation <- liftIO currentRollupGeneration
                rollup <- projectRollup ctx projectId
                liftIO $ insertRollupCache generation targetCommit projectId rollup
                pure rollup

projectRollup :: App es => ReadRepoContext -> Int -> ExceptT String (Eff es) Value
projectRollup ctx projectId = do
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
        certificatePaths = Map.map (T.pack . stepCertificate) allCertificates
    (rawStatuses, store) <- lift $ rawStatusesFor certificatePaths
    resolved <- lift $ resolveStatusesBatched store rawStatuses
    statuses <- lift $ markBuiltOutputs allCertificates resolved
    let rows = [rollupRow statuses certificates child (Map.findWithDefault Set.empty child subtrees) | child <- children]
    pure $ object ["project_id" .= projectId, "children" .= rows]

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
