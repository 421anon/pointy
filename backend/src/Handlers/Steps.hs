{-# LANGUAGE OverloadedStrings #-}

module Handlers.Steps (patchStepHandler, postStepHandler, noticesHandler) where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), catchError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.ProjectEntities (assignRecordToProject)
import Handlers.Projects (evaluateJsonToNix)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead, forkBroadcastStatusForStepProjectsAtHead)
import OutPaths (withWriteRepoTransaction)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, NoContent (..), throwError)
import Servant.Server (err400, err500, errBody)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixEvalJsonApplyInRepo, runNixEvalJsonInRepo, withReadRepoTransaction)

patchStepHandler :: Int -> LBS.ByteString -> Handler NoContent
patchStepHandler stepId jsonBody = do
    case TE.decodeUtf8' (LBS.toStrict jsonBody) of
        Left utf8Err -> throwError $ err400{errBody = TLE.encodeUtf8 $ TL.pack $ "Invalid UTF-8 in request body: " ++ show utf8Err}
        Right jsonText -> do
            result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
                evalRes <- ExceptT $ evaluateJsonToNix jsonText
                let stepsDir = worktreePath </> "steps"
                let outputPath = stepsDir </> show stepId ++ ".nix"
                liftIO $ TIO.writeFile outputPath (evalRes <> "\n")
                commitAndPushChanges ctx $ "Update step " ++ show stepId
            case result of
                Right _ -> do
                    liftIO $ forkBroadcastStatusForStepProjectsAtHead stepId
                    return NoContent
                Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

postStepHandler :: Maybe Int -> Maybe Int -> LBS.ByteString -> Handler LBS.ByteString
postStepHandler maybeProjectId maybeSourceId jsonBody = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        stepId <- saveStep ctx Nothing jsonBody
        liftIO $ copyClonedSrcFiles worktreePath maybeSourceId stepId
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        case maybeProjectId of
            Just projectId -> assignRecordToProject ctx projectId stepId
            Nothing -> return ()
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> runNixEvalJsonInRepo ctx ("#pointy.stepDefs." ++ show stepId)) $ \err -> do
            let outputPath = worktreePath </> "steps" </> show stepId ++ ".nix"
            _ <- liftIO $ readProcessWithExitCodeL "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
            throwError err
        let cloneNote = maybe "" (\srcId -> " (clone of " ++ show srcId ++ ")") maybeSourceId
        commitAndPushChanges ctx $
            case maybeProjectId of
                Just projectId -> "Create step " ++ show stepId ++ cloneNote ++ " and assign to project " ++ show projectId
                Nothing -> "Create step " ++ show stepId ++ cloneNote
        return output
    case result of
        Right output -> do
            case maybeProjectId of
                Just projectId -> liftIO $ forkBroadcastProjectStatusAtHead projectId
                Nothing -> return ()
            return output
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

noticesHandler :: Int -> Maybe T.Text -> Handler LBS.ByteString
noticesHandler stepId mCommit = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath headCommit) -> do
        let targetCommit = maybe headCommit T.unpack mCommit
            ctx = ReadRepoContext repoPath targetCommit
            attr = "#pointy.steps." ++ show stepId
            applyExpr = "s: if s ? meta && s.meta ? pointy && s.meta.pointy ? notices then s.meta.pointy.notices else []"
        TLE.encodeUtf8 . TL.pack <$> runNixEvalJsonApplyInRepo ctx applyExpr attr
    case result of
        Right output -> return output
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

saveStep :: WriteRepoContext -> Maybe Int -> LBS.ByteString -> ExceptT String IO Int
saveStep (WriteRepoContext worktreePath) maybeId jsonBody = ExceptT $ do
    case TE.decodeUtf8' (LBS.toStrict jsonBody) of
        Left utf8Err -> return $ Left $ "Invalid UTF-8 in request body: " ++ show utf8Err
        Right jsonText -> do
            result <- evaluateJsonToNix jsonText
            case result of
                Left err -> return $ Left err
                Right nixText -> do
                    let stepsDir = worktreePath </> "steps"
                    stepId <- maybe (getNextStepId stepsDir) return maybeId
                    let outputPath = stepsDir </> show stepId ++ ".nix"
                    TIO.writeFile outputPath (nixText <> "\n")
                    return $ Right stepId

getNextStepId :: FilePath -> IO Int
getNextStepId stepsDir = do
    exists <- doesDirectoryExist stepsDir
    if not exists
        then return 1
        else do
            files <- listDirectory stepsDir
            let ids = mapMaybe (readMaybe . takeBaseName) files :: [Int]
            return $ if null ids then 1 else maximum ids + 1

{- | When a step is cloned, duplicate its source files so the new step starts
with its own editable copy under @srcFiles/<newStepId>@.
-}
copyClonedSrcFiles :: FilePath -> Maybe Int -> Int -> IO ()
copyClonedSrcFiles _ Nothing _ = return ()
copyClonedSrcFiles worktreePath (Just sourceId) newStepId = do
    let sourceDir = worktreePath </> "srcFiles" </> show sourceId
        destDir = worktreePath </> "srcFiles" </> show newStepId
    sourceExists <- doesDirectoryExist sourceDir
    when sourceExists $ copyDirectoryRecursive sourceDir destDir

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive sourceDir destDir = do
    createDirectoryIfMissing True destDir
    entries <- listDirectory sourceDir
    forM_ entries $ \entry -> do
        let sourcePath = sourceDir </> entry
            destPath = destDir </> entry
        isDir <- doesDirectoryExist sourcePath
        if isDir
            then copyDirectoryRecursive sourcePath destPath
            else copyFile sourcePath destPath
