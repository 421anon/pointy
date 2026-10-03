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
import Data.Aeson (Object, Result (..), Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
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
import Handlers.Projects (annotateRecordChildren, readRecordMtimes)
import ProjectFiles (RawProjectFile (..), loadRawProjectFilesAt)
import ProjectTree (ChildRef (..), normalizeProject, projectChildRefs)
import RollupCache (currentRollupGeneration, insertRollupCache, lookupRollupCache)
import Servant.Server (err500, errBody)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), runNixEvalJsonApplyInRepo, withReadRepoTransaction)

unfiledHandler :: Maybe T.Text -> AppM DynamicJson
unfiledHandler commit = do
    result <- lift $ withReadRepoTransaction $ \ctx -> do
        let targetCommit = maybe (readCommitHash ctx) T.unpack commit
            readCtx = ReadRepoContext (readRepoPath ctx) targetCommit
        membership <- projectMembership readCtx
        times <- liftIO $ readRecordMtimes (readRepoPath ctx) targetCommit
        steps <- unfiledStepEntries readCtx (Set.toList (linkedStepIds membership))
        let children = annotateRecordChildren times (toJSON (steps ++ unfiledProjectEntries membership))
        pure (encode (object ["children" .= children, "membership" .= membershipIndex membership]))
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

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

unfiledProjectEntries :: Map Int [ChildRef] -> [Value]
unfiledProjectEntries membership =
    [ object ["project" .= object ["id" .= projectId, "hidden" .= False, "sortKey" .= Null]]
    | projectId <- Set.toList unfiled
    ]
  where
    linkedProjects = Set.fromList [projectId | refs <- Map.elems membership, ProjectChild projectId <- refs]
    unfiled = Set.difference (Map.keysSet membership) (Set.insert 0 linkedProjects)

membershipIndex :: Map Int [ChildRef] -> Value
membershipIndex membership =
    object [Key.fromText (T.pack (show projectId)) .= map childRefJson refs | (projectId, refs) <- Map.toList membership]

childRefJson :: ChildRef -> Value
childRefJson (StepChild stepId) = object ["step" .= object ["id" .= stepId]]
childRefJson (ProjectChild projectId) = object ["project" .= object ["id" .= projectId]]

unfiledStepEntries :: (Eval :> es, IOE :> es) => ReadRepoContext -> [Int] -> ExceptT String (Eff es) [Value]
unfiledStepEntries ctx members = do
    output <- runNixEvalJsonApplyInRepo ctx expression "#pointy"
    case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
        Right steps -> pure steps
        Left err -> throwError ("Failed to parse the unfiled steps: " ++ err)
  where
    expression =
        "pointy: let\n"
            ++ "  members = [ " ++ unwords (map show members) ++ " ];\n"
            ++ "  stepIds = map (name: builtins.fromJSON name) (builtins.attrNames pointy.steps);\n"
            ++ "  unfiledIds = builtins.filter (id: !(builtins.elem id members)) stepIds;\n"
            ++ "  entry = id: let\n"
            ++ "    n = builtins.toString id;\n"
            ++ "    t = builtins.tryEval (pointy.steps.${n}.def);\n"
            ++ "  in if t.success then { step = { inherit id; hidden = false; sortKey = null; def = t.value; }; } else null;\n"
            ++ "in builtins.filter (entry: entry != null) (builtins.map entry unfiledIds)"

projectRollupHandler :: Int -> Maybe T.Text -> AppM DynamicJson
projectRollupHandler projectId commit = do
    result <- lift $ withReadRepoTransaction $ \ctx -> do
        let targetCommit = maybe (readCommitHash ctx) T.unpack commit
            readCtx = ReadRepoContext (readRepoPath ctx) targetCommit
        cached <- liftIO $ lookupRollupCache targetCommit projectId
        case cached of
            Just value -> pure (encode value)
            Nothing -> do
                generation <- liftIO currentRollupGeneration
                rollup <- projectRollup readCtx projectId
                liftIO $ insertRollupCache generation targetCommit projectId rollup
                pure (encode rollup)
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

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
