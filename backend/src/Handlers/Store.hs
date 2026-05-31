{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers.Store (listHandler, downloadHandler, storeFilesHandler, stepListHandler, stepDownloadHandler, stepRawHandler, stepExtrasHandler, DirEntry (..)) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, Value (Object), eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Types (mkStatus, status200)
import Network.Wai (Application, Response, ResponseReceived, responseFile, responseLBS)
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
    err500,
    runHandler,
    throwError,
 )
import qualified Servant.Types.SourceT as S
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (joinPath, normalise, splitPath, takeExtension, takeFileName, (</>))
import UserRepo (ReadRepoContext (..), runNixEvalJsonApplyInRepo, runNixEvalRawInRepo, userRepoPath, withReadRepoTransaction)

import System.IO (IOMode (..), withBinaryFile)

data DirEntry = DirEntry
    { name :: Text
    , isDir :: Bool
    , size :: Integer
    , viewable :: Bool
    , mimeType :: Maybe Text
    }
    deriving (Generic, Show, ToJSON)

-- | Resolve a step id + optional commit to a store output path.
resolveStepOutPath :: Int -> Maybe Text -> Handler Text
resolveStepOutPath stepId mCommit = do
    repoPath <- liftIO userRepoPath
    commitHash <- case mCommit of
        Just c -> return (unpack c)
        Nothing -> do
            result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return hash
            case result of
                Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("resolveStepOutPath: " ++ err))}
                Right h -> return h
    let ctx = ReadRepoContext repoPath commitHash
    result <- liftIO $ runExceptT $ runNixEvalRawInRepo ctx ("#pointy.steps." ++ show stepId ++ ".outPath")
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to resolve step outPath: " ++ err))}
        Right path -> return (T.pack path)

stepListHandler :: Int -> Maybe Text -> Maybe FilePath -> Handler [DirEntry]
stepListHandler stepId mCommit mRel = do
    outPath <- resolveStepOutPath stepId mCommit
    listHandler outPath mRel

stepDownloadHandler :: Int -> Maybe Text -> FilePath -> Handler (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (S.SourceT IO BS.ByteString))
stepDownloadHandler stepId mCommit rel = do
    outPath <- resolveStepOutPath stepId mCommit
    downloadHandler outPath rel

stepRawHandler :: Int -> Maybe Text -> [String] -> Tagged Handler Application
stepRawHandler stepId mCommit segments = Tagged $ \_ respond -> do
    result <- runHandler $ resolveStepOutPath stepId mCommit
    case result of
        Left err -> respond $ responseLBS (mkStatus (errHTTPCode err) (TE.encodeUtf8 (T.pack (errReasonPhrase err)))) (errHeaders err) (errBody err)
        Right outPathText -> do
            let outPathSegments = drop 1 (splitPath (T.unpack outPathText))
                allSegments = outPathSegments ++ segments
            storeFilesHandler' allSegments respond

storeFilesHandler' :: [String] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
storeFilesHandler' segments respond = do
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
        Left err -> respond $ responseLBS (mkStatus (errHTTPCode err) (TE.encodeUtf8 (T.pack (errReasonPhrase err)))) (errHeaders err) (errBody err)
        Right (path, mime) -> do
            let headers = [("Content-Type", TE.encodeUtf8 mime)]
            respond $ responseFile status200 headers path Nothing

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
    ".csv" -> Just "text/csv"
    ".tsv" -> Just "text/tab-separated-values"
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

{- | GET /step-files/extras?id=<stepId>&commit=<commit>&path=<dirPath>
Returns the meta.json for the given directory from the extras derivation.
Returns {} when no extras attr exists or when meta.json is absent.
Returns 500 when meta.json exists but is not a JSON object, or when the
Nix evaluation itself fails for any reason other than the extras
attribute being absent.
-}
stepExtrasHandler :: Int -> Maybe Text -> Maybe FilePath -> Handler LBS.ByteString
stepExtrasHandler stepId mCommit mDirPath = do
    repoPath <- liftIO userRepoPath
    commitHash <- resolveCommitHash mCommit
    let ctx = ReadRepoContext repoPath commitHash
        stepAttr = "#pointy.steps." ++ show stepId
        -- attrByPath returns null when any segment of the path is missing,
        -- so absent extras yields a JSON null without producing a Nix error.
        -- Genuine evaluation failures (bad commit, unknown step id, broken
        -- step expression) still surface as a Left from runNixEval...
        applyExpr = "(s: builtins.attrByPath [\"meta\" \"pointy\" \"extras\" \"outPath\"] null s)"
    extrasResult <- liftIO $ runExceptT $ runNixEvalJsonApplyInRepo ctx applyExpr stepAttr
    extrasPath <- case extrasResult of
        Left err ->
            throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to evaluate extras outPath: " ++ err))}
        Right output ->
            case eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Maybe FilePath) of
                Left err ->
                    throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to decode extras outPath: " ++ err))}
                Right Nothing ->
                    return Nothing
                Right (Just path) ->
                    return (Just path)
    case extrasPath of
        Nothing -> return "{}"
        Just extrasPath_ -> do
            assertNixStorePath extrasPath_
            let dirPath = fromMaybe "" mDirPath
                metaPath = normalise (extrasPath_ </> dirPath </> "meta.json")
            assertInside metaPath extrasPath_
            exists <- liftIO $ doesFileExist metaPath
            if not exists
                then return "{}"
                else do
                    sz <- liftIO $ getFileSize metaPath
                    when (sz > maxExtrasJsonBytes) $
                        throwError err400{errBody = "extras meta.json exceeds 10 MiB size limit"}
                    content <- liftIO $ LBS.readFile metaPath
                    case eitherDecode content of
                        Left err ->
                            throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("extras meta.json parse error: " ++ err))}
                        Right (Object _) -> return content
                        Right _ ->
                            throwError err500{errBody = "extras meta.json is not a JSON object"}
  where
    maxExtrasJsonBytes = 10 * 1024 * 1024 -- 10 MiB

resolveCommitHash :: Maybe Text -> Handler String
resolveCommitHash mCommit = case mCommit of
    Just c -> return (unpack c)
    Nothing -> do
        result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ h) -> return h
        case result of
            Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("resolveCommitHash: " ++ err))}
            Right h -> return h
