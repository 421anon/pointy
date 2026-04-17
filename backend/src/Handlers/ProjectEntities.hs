{-# LANGUAGE OverloadedStrings #-}

module Handlers.ProjectEntities (assignRecordHandler, assignRecordToProject, batchAssignRecordsHandler, unassignRecordHandler) where

import Control.Concurrent (forkIO)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Handlers.Statuses (broadcastProjectStatus)
import OutPaths (withWriteRepoTransaction)
import Servant (Handler, NoContent (..), err500, errBody, throwError)
import System.FilePath ((</>))
import UserRepo (WriteRepoContext (..), commitAndPushChanges, runNix)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Projects (evaluateJsonToNix)

assignRecordHandler :: Int -> Int -> Handler NoContent
assignRecordHandler projectId recordId = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        assignRecordToProject ctx projectId recordId
        commitAndPushChanges ctx $ "Assign record " ++ show recordId ++ " to project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            _ <- liftIO $ forkIO $ broadcastProjectStatus projectId Nothing
            return NoContent

assignRecordToProject :: WriteRepoContext -> Int -> Int -> ExceptT String IO ()
assignRecordToProject ctx projectId recordId =
    updateProjectNixFile ctx projectId (addRecord recordId)

batchAssignRecordsHandler :: Int -> [Int] -> Handler NoContent
batchAssignRecordsHandler projectId recordIds = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        updateProjectNixFile ctx projectId (addRecords recordIds)
        commitAndPushChanges ctx $ "Batch assign records " ++ show recordIds ++ " to project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            _ <- liftIO $ forkIO $ broadcastProjectStatus projectId Nothing
            return NoContent

unassignRecordHandler :: Int -> Int -> Handler NoContent
unassignRecordHandler projectId recordId = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        updateProjectNixFile ctx projectId (removeRecord recordId)
        commitAndPushChanges ctx $ "Unassign record " ++ show recordId ++ " from project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            _ <- liftIO $ forkIO $ broadcastProjectStatus projectId Nothing
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

updateProjectNixFile :: WriteRepoContext -> Int -> T.Text -> ExceptT String IO ()
updateProjectNixFile (WriteRepoContext worktreePath) projectId transformation = ExceptT $ do
    let nixFilePath = worktreePath </> "projects" </> show projectId ++ ".nix"
        nixExpr = "let orig = import " <> T.pack nixFilePath <> "; in " <> transformation

    runExceptT $ do
        output <- runNix ["eval", "--impure", "--json", "--expr", T.unpack nixExpr]
        nixResult <- ExceptT $ evaluateJsonToNix (T.pack output)
        liftIO $ TIO.writeFile nixFilePath (nixResult <> "\n")
