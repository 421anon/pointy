{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Projects (getProjectsHandler, patchProjectHandler, batchProjectOpsHandler, postProjectHandler, readRecordMtimes, annotateRecordChildren) where

import ApiTypes (DynamicJson (..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.Except (ExceptT, liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Object, Result (..), Value (..), eitherDecode, encode, fromJSON, toJSON, withObject, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Effectful (Eff, IOE, (:>))
import Effects (AppM, Eval)
import Certificates (evalProjectDefinition, evalProjectDefinitions, withWriteRepoTransaction)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead)
import Handlers.StepReview (ensureStepsUnreviewed)
import ProjectFiles (applyTreeOpsIn, applyTreeOpsInWith, nextProjectId, projectFilePath, rewriteNixFile, srcFilesPath, stepFilePath, valueToNix)
import ProjectTree (ChildRef (..), ProjectFields, TreeOp (..), TreePlan (..), TreeState (..), describeTreeOps, newProject)
import Servant (NoContent (..))
import Servant.Server (err400, err409, err500, errBody)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixEvalJsonApplyInRepo, withReadRepoTransaction)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getProjectsHandler :: Maybe T.Text -> AppM DynamicJson
getProjectsHandler commit = do
    result <- lift $ withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) -> do
        let targetCommit = maybe commitHash T.unpack commit
        projects <- evalProjectDefinitions (ReadRepoContext repoPath targetCommit)
        times <- liftIO $ readRecordMtimes repoPath targetCommit
        return $ encode (annotateRecordMtimes times projects)
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

data RecordTimes = RecordTimes
    { recordModifiedTimes :: Map.Map FilePath T.Text
    , recordCreatedTimes :: Map.Map FilePath T.Text
    }

{-# NOINLINE mtimeCacheRef #-}
mtimeCacheRef :: MVar [(String, RecordTimes)]
mtimeCacheRef = unsafePerformIO (newMVar [])

mtimeCacheLimit :: Int
mtimeCacheLimit = 8

readRecordMtimes :: FilePath -> String -> IO RecordTimes
readRecordMtimes repoPath commit = do
    cache <- readMVar mtimeCacheRef
    case lookup commit cache of
        Just times -> return times
        Nothing -> do
            times <- loadRecordMtimes repoPath commit
            modifyMVar_ mtimeCacheRef $ \entries ->
                return $ take mtimeCacheLimit $ (commit, times) : filter ((/= commit) . fst) entries
            return times

loadRecordMtimes :: FilePath -> String -> IO RecordTimes
loadRecordMtimes repoPath commit = do
    (code, out, _) <- runGitIn repoPath ["log", commit, "--pretty=tformat:%ct", "--name-status", "--", "steps/", "projects/"]
    return $ case code of
        ExitSuccess -> collect (foldl' step (T.empty, Map.empty, Map.empty) (lines out))
        ExitFailure _ -> RecordTimes Map.empty Map.empty
  where
    step (stamp, modified, created) line
        | null line = (stamp, modified, created)
        | otherwise = case words line of
            [status, path] | status == "A" ->
                ( stamp
                , Map.insertWith (\_ old -> old) path stamp modified
                , Map.insertWith (\_ old -> old) path stamp created
                )
            [_, path] -> (stamp, Map.insertWith (\_ old -> old) path stamp modified, created)
            [_, _, path] -> (stamp, Map.insertWith (\_ old -> old) path stamp modified, created)
            _ -> (utcStamp line, modified, created)
    collect (_, modified, created) = RecordTimes modified created

utcStamp :: String -> T.Text
utcStamp raw =
    maybe (T.pack raw) (T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . posixSecondsToUTCTime . fromIntegral) (readMaybe raw :: Maybe Integer)

annotateRecordMtimes :: RecordTimes -> Value -> Value
annotateRecordMtimes times = onObject (KeyMap.map (annotateProject times))

annotateRecordChildren :: RecordTimes -> Value -> Value
annotateRecordChildren times = onArray (V.map (annotateChild times))

annotateProject :: RecordTimes -> Value -> Value
annotateProject times = onObject (stampRecord "projects/" times . adjustKey "children" (onArray (V.map (annotateChild times))))

annotateChild :: RecordTimes -> Value -> Value
annotateChild times = onObject (adjustKey "step" (onObject (adjustKey "def" (onObject (stampRecord "steps/" times)))))

stampRecord :: String -> RecordTimes -> Object -> Object
stampRecord prefix times obj = case integerId obj of
    Just i ->
        withTime "createdAt" (Map.lookup path (recordCreatedTimes times)) $
            withTime "lastModifiedAt" (Map.lookup path (recordModifiedTimes times)) obj
      where
        path = prefix ++ show i ++ ".nix"
    Nothing -> obj

withTime :: Key.Key -> Maybe T.Text -> Object -> Object
withTime key iso obj = maybe obj (\value -> KeyMap.insert key (String value) obj) iso

integerId :: Object -> Maybe Int
integerId obj = KeyMap.lookup "id" obj >>= \v -> case fromJSON v :: Result Int of Success i -> Just i; _ -> Nothing

onObject :: (Object -> Object) -> Value -> Value
onObject f v = case v of Object o -> Object (f o); _ -> v

onArray :: (V.Vector Value -> V.Vector Value) -> Value -> Value
onArray f v = case v of Array a -> Array (f a); _ -> v

adjustKey :: Key.Key -> (Value -> Value) -> Object -> Object
adjustKey k f m = maybe m (\v -> KeyMap.insert k (f v) m) (KeyMap.lookup k m)

patchProjectHandler :: Int -> ProjectFields -> AppM NoContent
patchProjectHandler projectId fields = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        rewriteProjectFile ctx projectId (replaceProjectFields fields)
        commitAndPushChanges ctx $ "Update project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

replaceProjectFields :: ProjectFields -> T.Text
replaceProjectFields fields = "builtins.removeAttrs orig [ \"preset\" \"templates\" ] // " <> valueToNix (toJSON fields)

data StepDependencies = StepDependencies
    { dependencyStepId :: Int
    , dependencyStepIds :: [Int]
    }

instance FromJSON StepDependencies where
    parseJSON = withObject "StepDependencies" $ \fields ->
        StepDependencies <$> fields .: "id" <*> fields .: "deps"

batchProjectOpsHandler :: [TreeOp] -> AppM NoContent
batchProjectOpsHandler [] =
    throwError $ err400{errBody = "Empty project tree batch"}
batchProjectOpsHandler ops = do
    result <- lift $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        let validate state plan = do
                ensureStepsUnreviewed ctx (planDeletedSteps plan)
                ensureDeletedStepsUnused ctx state (planDeletedSteps plan)
        (_, plan) <- applyTreeOpsInWith worktreePath validate ops
        removeDeletedFiles worktreePath plan
        commitAndPushChanges ctx (describeTreeOps ops)
        return (planChangedChildren plan)
    case result of
        Right changed -> do
            liftIO $ mapM_ forkBroadcastProjectStatusAtHead (Set.toList changed)
            return NoContent
        Left err -> throwError $ err409{errBody = TLE.encodeUtf8 (TL.pack err)}

ensureDeletedStepsUnused :: (Eval :> es) => WriteRepoContext -> TreeState -> [Int] -> ExceptT String (Eff es) ()
ensureDeletedStepsUnused _ _ [] = return ()
ensureDeletedStepsUnused ctx state deleted = do
    let deletedSet = Set.fromList deleted
        remaining = foldr Set.delete (treeSteps state) deleted
    dependencies <- evaluatedStepDependencies ctx (Set.toList remaining)
    let blocked =
            [ (stepId, dependencyId)
            | stepId <- Set.toList remaining
            , dependencyId <- Map.findWithDefault [] stepId dependencies
            , Set.member dependencyId deletedSet
            ]
    case blocked of
        [] -> return ()
        ((stepId, dependencyId) : _) ->
            throwError $ "Step " ++ show stepId ++ " depends on step " ++ show dependencyId ++ ", so it cannot be deleted."

evaluatedStepDependencies :: (Eval :> es) => WriteRepoContext -> [Int] -> ExceptT String (Eff es) (Map.Map Int [Int])
evaluatedStepDependencies ctx remaining = do
    output <- runNixEvalJsonApplyInRepo ctx (stepDependenciesExpression remaining) "#pointy.steps"
    entries <- liftEither $ either (Left . ("Failed to evaluate step dependencies: " ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))
    return $ Map.fromList [(dependencyStepId entry, dependencyStepIds entry) | entry <- entries]

stepDependenciesExpression :: [Int] -> String
stepDependenciesExpression ids =
    "steps: let ids = [ "
        ++ unwords (map show ids)
        ++ " ]; keep = builtins.filter (name: builtins.elem (builtins.fromJSON name) ids) (builtins.attrNames steps); "
        ++ "in builtins.map (name: let attempt = builtins.tryEval ((builtins.getAttr name steps).dependencies or []); "
        ++ "guarded = if attempt.success then (let value = builtins.tryEval (builtins.deepSeq attempt.value (builtins.map builtins.fromJSON attempt.value)); in if value.success then value.value else []) else []; "
        ++ "in { id = builtins.fromJSON name; deps = guarded; }) keep"

removeDeletedFiles :: (IOE :> es) => FilePath -> TreePlan -> ExceptT String (Eff es) ()
removeDeletedFiles worktreePath plan = do
    mapM_ (gitRemove . projectFilePath worktreePath) (planDeletedProjects plan)
    mapM_ (\stepId -> gitRemove (stepFilePath worktreePath stepId) >> gitRemove (srcFilesPath worktreePath stepId)) (planDeletedSteps plan)
  where
    gitRemove path = do
        (code, _, err) <- liftIO $ runGitIn worktreePath ["rm", "-rf", "--ignore-unmatch", path]
        case code of
            ExitSuccess -> return ()
            ExitFailure _ -> throwError ("git rm failed for " ++ path ++ ": " ++ err)

postProjectHandler :: Maybe Int -> ProjectFields -> AppM DynamicJson
postProjectHandler maybeParentId fields = do
    let parentId = fromMaybe 0 maybeParentId
    result <- lift $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        projectId <- liftIO $ nextProjectId worktreePath
        liftIO $ TIO.writeFile (projectFilePath worktreePath projectId) (valueToNix (newProject fields) <> "\n")
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        _ <- applyTreeOpsIn worktreePath [TreeLink parentId (ProjectChild projectId)]
        output <- TLE.encodeUtf8 . TL.pack <$> evalProjectDefinition ctx projectId
        commitAndPushChanges ctx $ "Create project " ++ show projectId ++ " in project " ++ show parentId
        return output
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

rewriteProjectFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> T.Text -> ExceptT String (Eff es) ()
rewriteProjectFile (WriteRepoContext worktreePath) projectId =
    rewriteNixFile (projectFilePath worktreePath projectId)
