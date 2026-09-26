{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CertificateStore (
    lookupCertificate,
    storeCertificate,
) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

lookupCertificate :: Text -> IO (Maybe FilePath)
lookupCertificate key = do
    path <- certificatePath key
    contents <- TIO.readFile path `catch` \(_ :: SomeException) -> pure ""
    let certificate = filter (not . isSpace) (T.unpack contents)
    pure $ if isCertificatePath certificate then Just certificate else Nothing

isCertificatePath :: String -> Bool
isCertificatePath certificate =
    "/nix/store/" `isPrefixOf` certificate && length certificate > length ("/nix/store/" :: String)

storeCertificate :: Text -> FilePath -> IO ()
storeCertificate key certificate = do
    present <- lookupCertificate key
    when (isNothing present) $ do
        path <- certificatePath key
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
