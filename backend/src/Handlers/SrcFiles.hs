{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.SrcFiles (listSrcFilesHandler, downloadSrcFilesHandler, seekSrcFilesHandler, getUserRepoInfoHandler, UserRepoInfo (..)) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Store (DirEntry, FileChunk, downloadHandler, listHandler, parseSeekOffset, seekHandler)
import Servant (Handler, Header, Headers, ServerError (..), err500, throwError)
import qualified Servant.Types.SourceT as S
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import UserRepo (runNixEvalRawInRepo, withReadRepoTransaction)

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

getSrcFilesBasePath :: Handler Text
getSrcFilesBasePath = do
    result <- liftIO $ withReadRepoTransaction $ \ctx -> do
        output <- runNixEvalRawInRepo ctx "#pointy.srcFiles"
        return $ T.strip (T.pack output)
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to evaluate pointy.srcFiles: " <> err))}
        Right path -> return path

getStepSrcFilesPath :: Int -> Handler FilePath
getStepSrcFilesPath stepId = do
    basePath <- getSrcFilesBasePath
    return (T.unpack basePath </> show stepId)

listSrcFilesHandler :: Int -> Maybe FilePath -> Handler [DirEntry]
listSrcFilesHandler stepId mRel = do
    fullBasePath <- getStepSrcFilesPath stepId
    exists <- liftIO $ doesDirectoryExist fullBasePath
    if exists
        then listHandler (T.pack fullBasePath) mRel
        else return []

downloadSrcFilesHandler :: Int -> FilePath -> Handler (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
downloadSrcFilesHandler stepId rel = do
    fullBasePath <- getStepSrcFilesPath stepId
    downloadHandler (T.pack fullBasePath) rel

seekSrcFilesHandler :: Int -> FilePath -> Maybe Int -> Maybe Int -> Int -> Handler FileChunk
seekSrcFilesHandler stepId rel line byteOffset bytes = do
    offset <- parseSeekOffset line byteOffset bytes
    fullBasePath <- getStepSrcFilesPath stepId
    seekHandler (T.pack fullBasePath) rel offset bytes
