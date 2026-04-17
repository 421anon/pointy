{-# LANGUAGE OverloadedStrings #-}

module Handlers.Steps (patchStepHandler, postStepHandler) where

import Control.Concurrent (forkIO)
import Control.Monad.Except (ExceptT (..), catchError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.ProjectEntities (assignRecordToProject)
import Handlers.Projects (evaluateJsonToNix)
import Handlers.Statuses (broadcastProjectStatus, broadcastStatusForStepProjects)
import OutPaths (withWriteRepoTransaction)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, NoContent (..), throwError)
import Servant.Server (err400, errBody)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)
import UserRepo (WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixInRepo)

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
                    _ <- liftIO $ forkIO $ broadcastStatusForStepProjects stepId Nothing
                    return NoContent
                Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

postStepHandler :: Maybe Int -> LBS.ByteString -> Handler LBS.ByteString
postStepHandler maybeProjectId jsonBody = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        stepId <- saveStep ctx Nothing jsonBody
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        case maybeProjectId of
            Just projectId -> assignRecordToProject ctx projectId stepId
            Nothing -> return ()
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> runNixInRepo ctx ["eval", "--json"] ("#pointy.stepDefs." ++ show stepId)) $ \err -> do
            let outputPath = worktreePath </> "steps" </> show stepId ++ ".nix"
            _ <- liftIO $ readProcessWithExitCodeL "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
            throwError err
        commitAndPushChanges ctx $
            case maybeProjectId of
                Just projectId -> "Create step " ++ show stepId ++ " and assign to project " ++ show projectId
                Nothing -> "Create step " ++ show stepId
        return output
    case result of
        Right output -> do
            case maybeProjectId of
                Just projectId -> do
                    _ <- liftIO $ forkIO $ broadcastProjectStatus projectId Nothing
                    return ()
                Nothing -> return ()
            return output
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

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
