{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ProjectEntities (assignRecordHandler, assignRecordToProject, batchAssignRecordsHandler, unassignRecordHandler) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Effectful (Eff, IOE, (:>))
import Effects (AppM, Eval)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead)
import Handlers.StepReview (ensureStepUnreviewed)
import Certificates (withWriteRepoTransaction)
import Servant (NoContent (..), err409, err500, errBody, throwError)
import System.FilePath ((</>))
import UserRepo (WriteRepoContext (..), commitAndPushChanges)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Projects (rewriteNixFile)

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

unassignRecordHandler :: Int -> Int -> AppM NoContent
unassignRecordHandler projectId recordId = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        ensureStepUnreviewed ctx recordId
        updateProjectNixFile ctx projectId (removeRecord recordId)
        commitAndPushChanges ctx $ "Unassign record " ++ show recordId ++ " from project " ++ show projectId
    case result of
        Left err -> throwError err409{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

addRecord :: Int -> T.Text
addRecord recordId =
    "orig // { steps = orig.steps ++ [{ hidden = false; id = " <> T.pack (show recordId) <> "; sortKey = null; }]; }"

addRecords :: [Int] -> T.Text
addRecords recordIds =
    let newSteps = T.intercalate " " $ map (\id_ -> "{ hidden = false; id = " <> T.pack (show id_) <> "; sortKey = null; }") recordIds
     in "orig // { steps = orig.steps ++ [ " <> newSteps <> " ]; }"

removeRecord :: Int -> T.Text
removeRecord recordId =
    "orig // { steps = builtins.filter (s: s.id != " <> T.pack (show recordId) <> ") orig.steps; }"

updateProjectNixFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> T.Text -> ExceptT String (Eff es) ()
updateProjectNixFile (WriteRepoContext worktreePath) projectId =
    rewriteNixFile (worktreePath </> "projects" </> show projectId ++ ".nix")
