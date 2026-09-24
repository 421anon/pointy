{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.SrcFiles (getStepSrcFilesPath, listSrcFilesHandler, downloadSrcFilesHandler, seekSrcFilesHandler, srcRawHandler, saveSrcFileHandler, createSrcFileHandler, deleteSrcFileHandler, getUserRepoInfoHandler, UserRepoInfo (..)) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON, eitherDecode)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effectful (Eff, IOE, (:>))
import EffectRunner (runAppEffects)
import Effects (AppM, Eval)
import GHC.Generics (Generic)
import Handlers.Store (DirEntry, FileChunk, downloadHandler, fromRawBase, listHandler, parseSeekOffset, seekHandler)
import Handlers.StepReview (ensureStepUnreviewed)
import Network.HTTP.Types (mkStatus)
import Network.Wai (Application, responseLBS)
import OutPaths (withWriteRepoTransaction)
import Servant (Header, Headers, NoContent (..), ServerError (..), Tagged (..), err400, err404, err409, err500, errBody, throwError)
import qualified Servant.Types.SourceT as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, removeFile)
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, commitContext, runNixEvalJsonApplyInRepo, withReadRepoTransaction)

data UserRepoInfo = UserRepoInfo
    { url :: Text
    , branch :: Text
    }
    deriving (Generic, ToJSON)

getUserRepoInfoHandler :: AppM UserRepoInfo
getUserRepoInfoHandler = do
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
    return $ UserRepoInfo (userRepoUrl userRepo) (userRepoBranch userRepo)

resolveStepSrcFiles :: (IOE :> es, Eval :> es) => Maybe Text -> Int -> Eff es (Either ServerError (Maybe FilePath))
resolveStepSrcFiles mCommit stepId = do
    result <-
        withReadRepoTransaction $ \ctx -> do
            target <- maybe (pure ctx) (\commit -> ExceptT $ liftIO $ runExceptT $ commitContext (readRepoPath ctx) commit) mCommit
            output <- runNixEvalJsonApplyInRepo target "step: step.srcFiles or null" ("#pointy.steps." ++ show stepId)
            either (throwError . ("Failed to decode the source files path: " ++)) pure $
                eitherDecode (TLE.encodeUtf8 (TL.pack output))
    pure $ either (Left . srcFilesError) Right result
  where
    srcFilesError err = err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to evaluate the source files of step " ++ show stepId ++ ": " ++ err))}

noSrcFiles :: Int -> ServerError
noSrcFiles stepId = err404{errBody = TLE.encodeUtf8 (TL.pack ("Step " ++ show stepId ++ " has no source files"))}

getStepSrcFilesPath :: Maybe Text -> Int -> AppM FilePath
getStepSrcFilesPath mCommit stepId =
    lift (resolveStepSrcFiles mCommit stepId) >>= either throwError (maybe (throwError (noSrcFiles stepId)) return)

listSrcFilesHandler :: Int -> Maybe Text -> Maybe FilePath -> AppM [DirEntry]
listSrcFilesHandler stepId mCommit mRel =
    lift (resolveStepSrcFiles mCommit stepId) >>= either throwError (maybe (return []) (\basePath -> listHandler (T.pack basePath) mRel))

downloadSrcFilesHandler :: Int -> Maybe Text -> FilePath -> AppM (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
downloadSrcFilesHandler stepId mCommit rel = do
    basePath <- getStepSrcFilesPath mCommit stepId
    downloadHandler (T.pack basePath) rel


srcRawHandler :: Int -> Maybe Text -> FilePath -> Tagged AppM Application
srcRawHandler stepId mCommit rel =
    Tagged $ \request respond -> do
        resolution <- runAppEffects (resolveStepSrcFiles mCommit stepId)
        case resolution >>= maybe (Left (noSrcFiles stepId)) Right of
            Left err -> respond $ responseLBS (mkStatus (errHTTPCode err) (TE.encodeUtf8 (T.pack (errReasonPhrase err)))) (errHeaders err) (errBody err)
            Right basePath -> fromRawBase basePath (splitDirectories rel) request respond



seekSrcFilesHandler :: Int -> Maybe Text -> FilePath -> Maybe Int -> Maybe Int -> Int -> AppM FileChunk
seekSrcFilesHandler stepId mCommit rel line byteOffset bytes = do
    offset <- parseSeekOffset line byteOffset bytes
    basePath <- getStepSrcFilesPath mCommit stepId
    seekHandler (T.pack basePath) rel offset bytes


mutateSrcFile :: Int -> FilePath -> String -> ServerError -> (FilePath -> IO Bool) -> AppM NoContent
mutateSrcFile stepId rel verb falseErr action
    | isAbsolute rel || null segments || any (`elem` [".", ".."]) segments =
        throwError err400{errBody = "Invalid source file path"}
    | otherwise = do
        result <- lift $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
            ensureStepUnreviewed ctx stepId
            done <- liftIO $ action (worktreePath </> "srcFiles" </> relPath)
            when done $ commitAndPushChanges ctx (verb ++ " source file " ++ relPath)
            pure done
        case result of
            Left err -> throwError err409{errBody = TLE.encodeUtf8 (TL.pack err)}
            Right True -> pure NoContent
            Right False -> throwError falseErr
  where
    segments = splitDirectories rel
    relPath = show stepId </> rel

saveSrcFileHandler :: Int -> FilePath -> Text -> AppM NoContent
saveSrcFileHandler stepId rel content =
    mutateSrcFile stepId rel "Update" err404{errBody = "Source file does not exist"} $ \target -> do
        exists <- doesFileExist target
        when exists $ TIO.writeFile target content
        pure exists

createSrcFileHandler :: Int -> FilePath -> Text -> AppM NoContent
createSrcFileHandler stepId rel content =
    mutateSrcFile stepId rel "Create" err409{errBody = "Source file already exists"} $ \target -> do
        exists <- doesPathExist target
        unless exists $ do
            createDirectoryIfMissing True (takeDirectory target)
            TIO.writeFile target content
        pure (not exists)

deleteSrcFileHandler :: Int -> FilePath -> AppM NoContent
deleteSrcFileHandler stepId rel =
    mutateSrcFile stepId rel "Delete" err404{errBody = "Source file does not exist"} $ \target -> do
        exists <- doesFileExist target
        when exists $ removeFile target
        pure exists
