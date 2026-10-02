{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.ProjectEntities (applyChildChangesHandler, assignRecordHandler, assignRecordToProject, batchAddChildrenHandler) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (intercalate, nub)
import Data.Maybe (isJust)
import Effectful (Eff, IOE, (:>))
import Effects (AppM, Eval)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead)
import Handlers.StepReview (ensureStepsUnreviewed)
import Certificates (withWriteRepoTransaction)
import ProjectTree (ChildChanges (..), ChildRef (..), ChildUpdate (..), appendChildren, appendMissingChildren, applyChildChanges, describeChild)
import Servant (NoContent (..), err409, err500, errBody, throwError)
import UserRepo (WriteRepoContext, commitAndPushChanges)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Projects (rewriteProjectFile)

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
    rewriteProjectFile ctx projectId (appendChildren [StepChild recordId])

batchAddChildrenHandler :: Int -> [ChildRef] -> AppM NoContent
batchAddChildrenHandler projectId children = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        rewriteProjectFile ctx projectId (appendMissingChildren children)
        commitAndPushChanges ctx $ "Add " ++ describeChildren (nub children) ++ " to project " ++ show projectId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

applyChildChangesHandler :: Int -> ChildChanges -> AppM NoContent
applyChildChangesHandler projectId changes = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        ensureStepsUnreviewed ctx [stepId | StepChild stepId <- removedChildren changes]
        rewriteProjectFile ctx projectId (applyChildChanges changes)
        commitAndPushChanges ctx $ childChangesMessage projectId changes
    case result of
        Left err -> throwError err409{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> do
            liftIO $ forkBroadcastProjectStatusAtHead projectId
            return NoContent

childChangesMessage :: Int -> ChildChanges -> String
childChangesMessage projectId (ChildChanges updates removals) =
    "Update children of project " ++ show projectId ++ ": " ++ intercalate "; " (concatMap describe groups)
  where
    groups =
        [ ("hide", [updatedChild update | update <- updates, updatedHidden update == Just True])
        , ("show", [updatedChild update | update <- updates, updatedHidden update == Just False])
        , ("reorder", [updatedChild update | update <- updates, isJust (updatedSortKey update)])
        , ("remove", removals)
        ]
    describe (_, []) = []
    describe (verb, children) = [verb ++ " " ++ describeChildren children]

describeChildren :: [ChildRef] -> String
describeChildren = intercalate ", " . map describeChild
