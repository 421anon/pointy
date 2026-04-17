{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers.Store (listHandler, downloadHandler, storeFilesHandler, DirEntry (..)) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Types (mkStatus, status200)
import Network.Wai (Application, responseFile, responseLBS)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (
    Handler,
    Header,
    Headers,
    ServerError (..),
    Tagged (..),
    addHeader,
    err400,
    err404,
    runHandler,
    throwError,
 )
import qualified Servant.Types.SourceT as S
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (joinPath, normalise, splitPath, takeExtension, takeFileName, (</>))
import System.IO (IOMode (..), withBinaryFile)

data DirEntry = DirEntry
    { name :: Text
    , isDir :: Bool
    , size :: Integer
    , viewable :: Bool
    , mimeType :: Maybe Text
    }
    deriving (Generic, Show, ToJSON)

listHandler :: Text -> Maybe FilePath -> Handler [DirEntry]
listHandler outPathText mRel = do
    let basePath = T.unpack outPathText
    assertNixStorePath basePath
    let rel = fromMaybe "" mRel
        absPath = normalise (basePath </> rel)
    assertInside absPath basePath
    names <- liftIO $ listDirectory absPath
    liftIO $ mapConcurrently (buildDirEntry absPath) names

buildDirEntry :: FilePath -> FilePath -> IO DirEntry
buildDirEntry absPath n = do
    let p = absPath </> n
    isD <- doesDirectoryExist p
    sz <- if isD then pure 0 else getFileSize p
    (isViewable, mime) <-
        if isD
            then pure (False, Nothing)
            else checkViewableAndMime p sz
    pure $ DirEntry (T.pack n) isD (fromIntegral sz) isViewable mime

downloadHandler :: Text -> FilePath -> Handler (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
downloadHandler outPathText rel = do
    let basePath = T.unpack outPathText
    assertNixStorePath basePath
    let absPath = normalise (basePath </> rel)
        filename = T.pack $ takeFileName rel
        disposition = "attachment; filename=\"" <> filename <> "\""
    assertInside absPath basePath
    isFile <- liftIO $ doesFileExist absPath
    unless isFile $ throwError err404
    fileSize <- liftIO $ getFileSize absPath
    let source = readFileChunked absPath
    return $ addHeader disposition $ addHeader fileSize source

readFileChunked :: FilePath -> S.SourceT IO BS.ByteString
readFileChunked path = S.SourceT $ \k ->
    withBinaryFile path ReadMode $ \h ->
        k $ readChunks h
  where
    chunkSize = 262144 -- 256 KiB
    readChunks h = S.fromActionStep BS.null (BS.hGet h chunkSize)

getMimeType :: FilePath -> IO (Maybe Text)
getMimeType path = do
    (exitCode, output, _) <- readProcessWithExitCodeL "file" ["-b", "-L", "--mime-type", path] ""
    case exitCode of
        ExitSuccess -> pure $ Just (T.strip $ T.pack output)
        ExitFailure _ -> pure Nothing

mimeTypeByExtension :: FilePath -> Maybe Text
mimeTypeByExtension path = case takeExtension path of
    ".css" -> Just "text/css"
    ".js" -> Just "application/javascript"
    ".mjs" -> Just "application/javascript"
    ".svg" -> Just "image/svg+xml"
    ".woff" -> Just "font/woff"
    ".woff2" -> Just "font/woff2"
    ".ttf" -> Just "font/ttf"
    ".eot" -> Just "application/vnd.ms-fontobject"
    ".json" -> Just "application/json"
    ".xml" -> Just "application/xml"
    ".html" -> Just "text/html"
    ".htm" -> Just "text/html"
    _ -> Nothing

resolvedMimeType :: FilePath -> IO Text
resolvedMimeType path = case mimeTypeByExtension path of
    Just mime -> pure mime
    Nothing -> do
        detected <- getMimeType path
        pure $ fromMaybe "application/octet-stream" detected

storeFilesHandler :: [String] -> Tagged Handler Application
storeFilesHandler segments = Tagged $ \_ respond -> do
    result <- runHandler $ do
        unless (length segments >= 3) $
            throwError err400{errBody = "Invalid store path"}
        let absPath = normalise $ "/" ++ intercalate "/" segments
            basePath = normalise $ "/" ++ intercalate "/" (take 3 segments)
        assertNixStorePath absPath
        assertInside absPath basePath
        exists <- liftIO $ doesFileExist absPath
        unless exists $ throwError err404
        mime <- liftIO $ resolvedMimeType absPath
        pure (absPath, mime)
    case result of
        Left err -> respond $ responseLBS (mkStatus (errHTTPCode err) (TE.encodeUtf8 $ T.pack $ errReasonPhrase err)) (errHeaders err) (errBody err)
        Right (path, mime) -> do
            let headers = [("Content-Type", TE.encodeUtf8 mime)]
            respond $ responseFile status200 headers path Nothing

checkViewableAndMime :: FilePath -> Integer -> IO (Bool, Maybe Text)
checkViewableAndMime path sz = do
    mType <- getMimeType path
    if sz > 15728640 -- 15 MiB
        then pure (False, mType)
        else pure (maybe False isReadableMimeType mType, mType)

isReadableMimeType :: Text -> Bool
isReadableMimeType mimeType =
    any
        (`T.isPrefixOf` mimeType)
        [ "text/"
        , "application/json"
        , "application/xml"
        , "application/javascript"
        , "application/x-javascript"
        , "application/typescript"
        , "application/x-httpd-php"
        , "application/x-sh"
        , "application/x-shellscript"
        ]

assertNixStorePath :: FilePath -> Handler ()
assertNixStorePath path =
    unless ("/nix/store/" `isPrefixOf` path) $
        throwError err400{errBody = "Invalid store path"}

assertInside :: FilePath -> FilePath -> Handler ()
assertInside path base =
    unless (joinPath (splitPath (normalise base)) `isPrefixOf` joinPath (splitPath (normalise path))) $
        throwError err400{errBody = "Path traversal not allowed"}
