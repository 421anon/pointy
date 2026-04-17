{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.SrcFiles (listSrcFilesHandler, downloadSrcFilesHandler, getUserRepoInfoHandler, UserRepoInfo (..)) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Store (DirEntry, downloadHandler, listHandler)
import Servant (Handler, Header, Headers, ServerError (..), err500, throwError)
import qualified Servant.Types.SourceT as S
import System.FilePath ((</>))
import UserRepo (runNixInRepo, withReadRepoTransaction)

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
        output <- runNixInRepo ctx ["eval", "--raw"] "#pointy.srcFiles"
        return $ T.strip (T.pack output)
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to evaluate pointy.srcFiles: " <> err))}
        Right path -> return path

listSrcFilesHandler :: Int -> Maybe FilePath -> Handler [DirEntry]
listSrcFilesHandler stepId mRel = do
    basePath <- getSrcFilesBasePath
    let fullBasePath = T.unpack basePath </> show stepId
    listHandler (T.pack fullBasePath) mRel

downloadSrcFilesHandler :: Int -> FilePath -> Handler (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
downloadSrcFilesHandler stepId rel = do
    basePath <- getSrcFilesBasePath
    let fullBasePath = T.unpack basePath </> show stepId
    downloadHandler (T.pack fullBasePath) rel
