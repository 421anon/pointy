{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CertificateStore (
    lookupCertificate,
    storeCertificate,
) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

lookupCertificate :: Text -> IO (Maybe FilePath)
lookupCertificate key = do
    path <- certificatePath key
    contents <- readFile path `catch` \(_ :: SomeException) -> pure ""
    let certificate = filter (not . isSpace) contents
    pure $ if null certificate then Nothing else Just certificate

storeCertificate :: Text -> FilePath -> IO ()
storeCertificate key certificate = do
    path <- certificatePath key
    exists <- doesFileExist path
    when (not exists) $ do
        dir <- certificateStoreDir
        createDirectoryIfMissing True dir
        (tempPath, handle) <- openTempFile dir (storeFile key ++ ".tmp")
        hClose handle
        writeFile tempPath (certificate ++ "\n")
        renameFile tempPath path
            `catch` \(_ :: SomeException) -> removeFile tempPath `catch` \(_ :: SomeException) -> pure ()

certificatePath :: Text -> IO FilePath
certificatePath key = do
    dir <- certificateStoreDir
    pure $ dir </> storeFile key

certificateStoreDir :: IO FilePath
certificateStoreDir = do
    home <- getHomeDirectory
    pure $ home </> ".local" </> "state" </> "pointy" </> "certificates"

storeFile :: Text -> String
storeFile = map sanitize . T.unpack
  where
    sanitize char
        | isAlphaNum char || char == '-' || char == '_' || char == '.' = char
        | otherwise = '_'
