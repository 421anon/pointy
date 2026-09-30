{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ProjectEntities (StepChanges (..), applyStepChangesHandler, assignRecordHandler, assignRecordToProject, batchAssignRecordsHandler) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.List (intercalate)
import qualified Data.Text as T
import Effectful (Eff, IOE, (:>))
import Effects (AppM, Eval)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead)
import Handlers.StepReview (ensureStepsUnreviewed)
import Certificates (withWriteRepoTransaction)
import Servant (NoContent (..), err409, err500, errBody, throwError)
import System.FilePath ((</>))
import UserRepo (WriteRepoContext (..), commitAndPushChanges)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Projects (rewriteNixFile)

data StepChanges = StepChanges
    { hiddenSteps :: [Int]
    , shownSteps :: [Int]
    , removedSteps :: [Int]
    }
    deriving (Show)

instance FromJSON StepChanges where
    parseJSON = withObject "StepChanges" $ \obj ->
        StepChanges
            <$> obj .: "hide"
            <*> obj .: "show"
            <*> obj .: "remove"

assignRecordHandler :: Int -> Int -> AppM NoContent
assignRecordHandler projectId recordId = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        assignRecordToProject ctx projectId recordId
        commitAndPushChanges ctx $ "Assign record " ++ show recordId ++ " to project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

assignRecordToProject :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> Int -> ExceptT String (Eff es) ()
assignRecordToProject ctx projectId recordId =
    updateProjectNixFile ctx projectId (addRecord recordId)

batchAssignRecordsHandler :: Int -> [Int] -> AppM NoContent
batchAssignRecordsHandler projectId recordIds = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        updateProjectNixFile ctx projectId (addRecords recordIds)
        commitAndPushChanges ctx $ "Batch assign records " ++ show recordIds ++ " to project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

applyStepChangesHandler :: Int -> StepChanges -> AppM NoContent
applyStepChangesHandler projectId changes = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        ensureStepsUnreviewed ctx (removedSteps changes)
        updateProjectNixFile ctx projectId (applyStepChanges changes)
        commitAndPushChanges ctx $ stepChangesMessage projectId changes
    case result of
        Left err -> throwError err409{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

stepChangesMessage :: Int -> StepChanges -> String
stepChangesMessage projectId changes =
    "Update steps of project " ++ show projectId ++ ": " ++ intercalate "; " (concatMap describe [("hide", hiddenSteps), ("show", shownSteps), ("remove", removedSteps)])
  where
    describe (verb, field) = case field changes of
        [] -> []
        stepIds -> [verb ++ " " ++ intercalate ", " (map show stepIds)]

applyStepChanges :: StepChanges -> T.Text
applyStepChanges changes =
    "orig // { steps = map (s: s // (if builtins.elem s.id "
        <> nixIntList (hiddenSteps changes)
        <> " then { hidden = true; } else if builtins.elem s.id "
        <> nixIntList (shownSteps changes)
        <> " then { hidden = false; } else { })) (builtins.filter (s: !(builtins.elem s.id "
        <> nixIntList (removedSteps changes)
        <> ")) orig.steps); }"

nixIntList :: [Int] -> T.Text
nixIntList stepIds = "[ " <> T.unwords (map (T.pack . show) stepIds) <> " ]"

addRecord :: Int -> T.Text
addRecord recordId =
    "orig // { steps = orig.steps ++ [{ hidden = false; id = " <> T.pack (show recordId) <> "; sortKey = null; }]; }"

addRecords :: [Int] -> T.Text
addRecords recordIds =
    let newSteps = T.intercalate " " $ map (\id_ -> "{ hidden = false; id = " <> T.pack (show id_) <> "; sortKey = null; }") recordIds
     in "orig // { steps = orig.steps ++ [ " <> newSteps <> " ]; }"

updateProjectNixFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> T.Text -> ExceptT String (Eff es) ()
updateProjectNixFile (WriteRepoContext worktreePath) projectId =
    rewriteNixFile (worktreePath </> "projects" </> show projectId ++ ".nix")
