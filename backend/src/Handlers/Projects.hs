{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Projects (getProjectsHandler, patchProjectHandler, postProjectHandler, deleteProjectHandler, evaluateJsonToNix, RawJSON) where

import Control.Monad.Except (ExceptT (..), catchError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Maybe (mapMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Media ((//))
import OutPaths (withWriteRepoTransaction)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Accept (..), Handler, MimeRender (..), MimeUnrender (..), NoContent (..), throwError)
import Servant.Server (err400, err500, errBody)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixInRepo, withReadRepoTransaction)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix (nixEvalExpr, withNixContext)
import Nix.Expr.Shorthands (mkStr, mkSym, (@.), (@@))
import Nix.Normal (normalForm)
import Nix.Options (defaultOptions)
import Nix.Pretty (prettyNix, valueToExpr)
import Nix.Standard (runWithBasicEffectsIO)
import NixUtils (sortAttrSet)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

data RawJSON
instance Accept RawJSON where contentType _ = "application" // "json"
instance MimeRender RawJSON LB.ByteString where mimeRender _ = id
instance MimeUnrender RawJSON LB.ByteString where mimeUnrender _ = Right

getProjectsHandler :: Maybe T.Text -> Handler LB.ByteString
getProjectsHandler commit = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) -> do
        let targetCommit = maybe commitHash T.unpack commit
        output <- runNixInRepo (ReadRepoContext repoPath targetCommit) ["eval", "--json"] "#trotter.projects"
        return $ LB.fromStrict $ TE.encodeUtf8 $ T.pack output
    case result of
        Right output -> return output
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

patchProjectHandler :: Int -> LB.ByteString -> Handler NoContent
patchProjectHandler projectId jsonBody = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        _ <- saveProject ctx (Just projectId) jsonBody
        commitAndPushChanges ctx $ "Update project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

deleteProjectHandler :: Int -> Handler NoContent
deleteProjectHandler projectId = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        let outputPath = worktreePath </> "projects" </> show projectId ++ ".nix"
        _ <- liftIO $ readProcessWithExitCodeL "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
        commitAndPushChanges ctx $ "Delete project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

postProjectHandler :: LB.ByteString -> Handler LB.ByteString
postProjectHandler jsonBody = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        projectId <- saveProject ctx Nothing jsonBody
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> runNixInRepo ctx ["eval", "--json"] ("#trotter.projects." ++ show projectId)) $ \err -> do
            let outputPath = worktreePath </> "projects" </> show projectId ++ ".nix"
            _ <- liftIO $ readProcessWithExitCodeL "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
            throwError err
        commitAndPushChanges ctx $ "Create project " ++ show projectId
        return output
    case result of
        Right output -> return output
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

saveProject :: WriteRepoContext -> Maybe Int -> LB.ByteString -> ExceptT String IO Int
saveProject (WriteRepoContext worktreePath) maybeId jsonBody = ExceptT $ do
    case TE.decodeUtf8' (LB.toStrict jsonBody) of
        Left utf8Err -> return $ Left $ "Invalid UTF-8 in request body: " ++ show utf8Err
        Right jsonText -> do
            result <- evaluateJsonToNix jsonText
            case result of
                Left err -> return $ Left err
                Right nixText -> do
                    let projectsDir = worktreePath </> "projects"
                    projectId <- maybe (getNextProjectId projectsDir) return maybeId
                    let outputPath = projectsDir </> show projectId ++ ".nix"
                    TIO.writeFile outputPath (nixText <> "\n")
                    return $ Right projectId

getNextProjectId :: FilePath -> IO Int
getNextProjectId projectsDir = do
    exists <- doesDirectoryExist projectsDir
    if not exists
        then return 1
        else do
            files <- listDirectory projectsDir
            let ids = mapMaybe (readMaybe . takeBaseName) files :: [Int]
            return $ if null ids then 1 else maximum ids + 1

evaluateJsonToNix :: T.Text -> IO (Either String T.Text)
evaluateJsonToNix jsonText = do
    let fullExpr = mkSym "builtins" @. "fromJSON" @@ mkStr jsonText
    let opts = defaultOptions $ posixSecondsToUTCTime 0
    result <- runWithBasicEffectsIO opts $ withNixContext Nothing $ do
        val <- nixEvalExpr Nothing fullExpr
        nf <- normalForm val
        return $ valueToExpr nf
    let sortedResult = sortAttrSet result
        nixText = renderStrict $ layoutPretty defaultLayoutOptions $ prettyNix sortedResult
    return $ Right nixText
