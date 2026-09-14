{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.SrcFiles (getStepSrcFilesPath, listSrcFilesHandler, downloadSrcFilesHandler, seekSrcFilesHandler, srcRawHandler, saveSrcFileHandler, createSrcFileHandler, deleteSrcFileHandler, getUserRepoInfoHandler, UserRepoInfo (..)) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Store (DirEntry, FileChunk, downloadHandler, fromRawBase, listHandler, parseSeekOffset, seekHandler)
import Handlers.StepValidation (ensureStepUnvalidated)
import OutPaths (withWriteRepoTransaction)
import Network.Wai (Application)
import Servant (Handler, Header, Headers, NoContent (..), ServerError (..), Tagged (..), err400, err404, err409, err500, throwError)
import qualified Servant.Types.SourceT as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, removeFile)
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, commitContext, runNixEvalRawInRepo, withReadRepoTransaction)

data UserRepoInfo = UserRepoInfo
    { url :: Text
    , branch :: Text
    }
    deriving (Generic, ToJSON)

getUserRepoInfoHandler :: Handler UserRepoInfo
getUserRepoInfoHandler = do
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
    return $ UserRepoInfo (userRepoUrl userRepo) (userRepoBranch userRepo)

getSrcFilesBasePath :: Maybe Text -> Handler Text
getSrcFilesBasePath mCommit = do
    result <- liftIO $ withReadRepoTransaction $ \ctx -> do
        target <- maybe (pure ctx) (\commit -> commitContext (readRepoPath ctx) commit) mCommit
        output <- runNixEvalRawInRepo target "#pointy.srcFiles"
        return $ T.strip (T.pack output)
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to evaluate pointy.srcFiles: " <> err))}
        Right path -> return path

getStepSrcFilesPath :: Maybe Text -> Int -> Handler FilePath
getStepSrcFilesPath mCommit stepId = do
    basePath <- getSrcFilesBasePath mCommit
    return (T.unpack basePath </> show stepId)

listSrcFilesHandler :: Int -> Maybe Text -> Maybe FilePath -> Handler [DirEntry]
listSrcFilesHandler stepId mCommit mRel = do
    fullBasePath <- getStepSrcFilesPath mCommit stepId
    exists <- liftIO $ doesDirectoryExist fullBasePath
    if exists
        then listHandler (T.pack fullBasePath) mRel
        else return []

downloadSrcFilesHandler :: Int -> Maybe Text -> FilePath -> Handler (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
downloadSrcFilesHandler stepId mCommit rel = do
    fullBasePath <- getStepSrcFilesPath mCommit stepId
    downloadHandler (T.pack fullBasePath) rel


-- | Serves a source file inline (no download disposition) so HTML and other
-- renderable sources can be shown in preview iframes.
srcRawHandler :: Int -> Maybe Text -> FilePath -> Tagged Handler Application
srcRawHandler stepId mCommit rel =
    fromRawBase (getStepSrcFilesPath mCommit stepId) (splitDirectories rel)



seekSrcFilesHandler :: Int -> Maybe Text -> FilePath -> Maybe Int -> Maybe Int -> Int -> Handler FileChunk
seekSrcFilesHandler stepId mCommit rel line byteOffset bytes = do
    offset <- parseSeekOffset line byteOffset bytes
    fullBasePath <- getStepSrcFilesPath mCommit stepId
    seekHandler (T.pack fullBasePath) rel offset bytes


-- | Mutate a step's source file inside a write transaction; a 'False' result raises @falseErr@.
mutateSrcFile :: Int -> FilePath -> String -> ServerError -> (FilePath -> IO Bool) -> Handler NoContent
mutateSrcFile stepId rel verb falseErr action
    | isAbsolute rel || null segments || any (`elem` [".", ".."]) segments =
        throwError err400{errBody = "Invalid source file path"}
    | otherwise = do
        result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
            ensureStepUnvalidated ctx stepId
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

saveSrcFileHandler :: Int -> FilePath -> Text -> Handler NoContent
saveSrcFileHandler stepId rel content =
    mutateSrcFile stepId rel "Update" err404{errBody = "Source file does not exist"} $ \target -> do
        exists <- doesFileExist target
        when exists $ TIO.writeFile target content
        pure exists

createSrcFileHandler :: Int -> FilePath -> Text -> Handler NoContent
createSrcFileHandler stepId rel content =
    mutateSrcFile stepId rel "Create" err409{errBody = "Source file already exists"} $ \target -> do
        exists <- doesPathExist target
        unless exists $ do
            createDirectoryIfMissing True (takeDirectory target)
            TIO.writeFile target content
        pure (not exists)

deleteSrcFileHandler :: Int -> FilePath -> Handler NoContent
deleteSrcFileHandler stepId rel =
    mutateSrcFile stepId rel "Delete" err404{errBody = "Source file does not exist"} $ \target -> do
        exists <- doesFileExist target
        when exists $ removeFile target
        pure exists
