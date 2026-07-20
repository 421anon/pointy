{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers.Store (listHandler, downloadHandler, seekHandler, storeFilesHandler, stepListHandler, stepDownloadHandler, stepSeekHandler, stepRawHandler, stepBundleHandler, stepExtrasHandler, DirEntry (..), FileChunk, LineOffset, ByteOffset, fileChunkSize, maxViewableSize, checkViewableAndMime, parseSeekOffset) where

import ApiTypes (DynamicJson (..))

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value (Object), eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Generics (Generic)
import Handlers.RunStep (buildExtras)
import Network.HTTP.Types (mkStatus, status200)
import Network.Wai (Application, Response, ResponseReceived, responseFile, responseLBS)
import OutPaths (lookupCachedStepOutPath)
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
import System.Process (readProcessWithExitCode)
import UserRepo (ReadRepoContext (..), runNixEvalJsonApplyInRepo, runNixEvalRawInRepo, userRepoPath, withReadRepoTransaction)

import System.IO (IOMode (..), SeekMode (..), hSeek, withBinaryFile)

data DirEntry = DirEntry
    { name :: Text
    , isDir :: Bool
    , size :: Integer
    , viewable :: Bool
    , seekable :: Bool
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
    -- Consult the project outPath cache first (read-only, non-blocking).
    mCached <- liftIO $ lookupCachedStepOutPath (T.pack commitHash) stepId
    case mCached of
        Just path -> return path
        Nothing -> do
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

stepBundleHandler :: Int -> Text -> [String] -> Tagged Handler Application
stepBundleHandler stepId commit = stepRawHandler stepId (Just commit)

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
    (isViewable, isSeekable, mime) <-
        if isD
            then pure (False, False, Nothing)
            else checkViewableAndMime p sz
    pure $ DirEntry (T.pack n) isD (fromIntegral sz) isViewable isSeekable mime

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

fileChunkSize :: Int
fileChunkSize = 2 * 1024 * 1024

maxViewableSize :: Integer
maxViewableSize = 5 * 1024 * 1024

readFileChunked :: FilePath -> S.SourceT IO BS.ByteString
readFileChunked path = S.SourceT $ \k ->
    withBinaryFile path ReadMode $ \h ->
        k $ readChunks h
  where
    readChunks h = S.fromActionStep BS.null (BS.hGet h fileChunkSize)

getMimeType :: FilePath -> IO (Maybe Text)
getMimeType path = do
    (exitCode, output, _) <- readProcessWithExitCode "file" ["-b", "-L", "--mime-type", path] ""
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

checkViewableAndMime :: FilePath -> Integer -> IO (Bool, Bool, Maybe Text)
checkViewableAndMime path sz = do
    mType <- getMimeType path
    let isReadable = maybe False isReadableMimeType mType
        isSeekable = isReadable && sz > maxViewableSize
        isViewable = isReadable && sz <= maxViewableSize
    pure (isViewable, isSeekable, mType)

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
stepExtrasHandler :: Int -> Maybe Text -> Maybe FilePath -> Handler DynamicJson
stepExtrasHandler stepId mCommit mDirPath = do
    repoPath <- liftIO userRepoPath
    commitHash <- resolveCommitHash mCommit
    let ctx = ReadRepoContext repoPath commitHash
        stepAttr = "#pointy.steps." ++ show stepId
        -- The `?` dotted path checks each segment safely and short-circuits,
        -- so a missing meta.pointy.extras.outPath yields JSON null without a
        -- Nix error.  Genuine eval failures (bad commit, unknown step id,
        -- broken step expression) still surface as a Left from runNixEval...
        applyExpr = "(s: if s ? meta.pointy.extras.outPath then s.meta.pointy.extras.outPath else null)"
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
        Nothing -> return (DynamicJson "{}")
        Just extrasPath_ -> do
            assertNixStorePath extrasPath_
            let dirPath = fromMaybe "" mDirPath
                metaPath = normalise (extrasPath_ </> dirPath </> "meta.json")
            assertInside metaPath extrasPath_
            exists <- liftIO $ doesFileExist metaPath
            if not exists
                then do
                    -- meta.json is absent for one of two reasons:
                    --   (a) the extras derivation has not been realised yet, or
                    --   (b) it was realised but emits no metadata for this dir.
                    -- Enqueue a build in the background so subsequent requests
                    -- can pick up the metadata; buildExtras short-circuits to a
                    -- GC-root refresh when the derivation is already built, so
                    -- case (b) costs only one Nix eval and one squeue check.
                    liftIO $ void $ forkIO $ buildExtras ctx stepId
                    return (DynamicJson "{}")
                else do
                    sz <- liftIO $ getFileSize metaPath
                    when (sz > maxExtrasJsonBytes) $
                        throwError err400{errBody = "extras meta.json exceeds 10 MiB size limit"}
                    content <- liftIO $ LBS.readFile metaPath
                    case eitherDecode content of
                        Left err ->
                            throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("extras meta.json parse error: " ++ err))}
                        Right (Object _) -> return (DynamicJson content)
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

newtype LineOffset = LineOffset Int

instance ToJSON LineOffset where
    toJSON (LineOffset offset) = toJSON offset

newtype ByteOffset = ByteOffset Int

instance ToJSON ByteOffset where
    toJSON (ByteOffset offset) = toJSON offset

data FileChunk = FileChunk
    { content :: Text
    , startOffset :: ByteOffset
    , endOffset :: ByteOffset
    , startLine :: LineOffset
    , endLine :: LineOffset
    , eof :: Bool
    }
    deriving (Generic, ToJSON)

parseSeekOffset :: Maybe Int -> Maybe Int -> Int -> Handler (Either LineOffset ByteOffset)
parseSeekOffset line byteOffset bytes = do
    when (bytes == 0) $
        throwError err400{errBody = "bytes must not be zero"}
    when (bytes < -fileChunkSize || bytes > fileChunkSize) $
        throwError err400{errBody = TLE.encodeUtf8 (TL.pack ("abs(bytes) must be <= " ++ show fileChunkSize))}
    case (line, byteOffset) of
        (Just value, Nothing)
            | value < 1 -> throwError err400{errBody = "line must be >= 1"}
            | otherwise -> return (Left (LineOffset value))
        (Nothing, Just value)
            | value < 0 -> throwError err400{errBody = "offset must be >= 0"}
            | otherwise -> return (Right (ByteOffset value))
        _ -> throwError err400{errBody = "Specify exactly one of line or offset"}

stepSeekHandler :: Int -> Maybe Text -> FilePath -> Maybe Int -> Maybe Int -> Int -> Handler FileChunk
stepSeekHandler stepId mCommit rel line byteOffset bytes = do
    offset <- parseSeekOffset line byteOffset bytes
    outPath <- resolveStepOutPath stepId mCommit
    seekHandler outPath rel offset bytes

seekHandler :: Text -> FilePath -> Either LineOffset ByteOffset -> Int -> Handler FileChunk
seekHandler basePathText rel offset bytes = do
    let basePath = T.unpack basePathText
    assertNixStorePath basePath
    let absPath = normalise (basePath </> rel)
    assertInside absPath basePath
    isFile <- liftIO $ doesFileExist absPath
    unless isFile $ throwError err404
    fileSize <- liftIO $ getFileSize absPath
    case offset of
        Right (ByteOffset value)
            | fromIntegral value > fileSize ->
                throwError err400{errBody = "seek offset beyond end of file"}
        _ -> return ()
    liftIO $ seekFileChunk absPath offset bytes fileSize

seekFileChunk :: FilePath -> Either LineOffset ByteOffset -> Int -> Integer -> IO FileChunk
seekFileChunk path offset bytes fileSize = do
    (anchorOffset, anchorLine) <- case offset of
        Left line -> do
            (resolvedOffset, resolvedLine) <- findLineOffset path line fileSize
            return (resolvedOffset, Just resolvedLine)
        Right byteOffset -> return (byteOffset, Nothing)
    let ByteOffset anchorValue = anchorOffset
        rawStartOffset = ByteOffset (if bytes > 0 then anchorValue else max 0 (anchorValue + bytes))
    alignedStartOffset@(ByteOffset startValue) <- alignUTF8Start path rawStartOffset fileSize
    let requestedBytes =
            if bytes > 0 then bytes else anchorValue - startValue
        knownStartLine =
            if bytes > 0 then anchorLine else Nothing
    readSeekChunk path alignedStartOffset requestedBytes fileSize knownStartLine

findLineOffset :: FilePath -> LineOffset -> Integer -> IO (ByteOffset, LineOffset)
findLineOffset path (LineOffset targetLine) fileSize =
    withBinaryFile path ReadMode $ \h -> do
        let go currentOffset currentLine
                | currentLine >= targetLine = return (ByteOffset currentOffset, LineOffset currentLine)
                | currentOffset >= fromIntegral fileSize = return (ByteOffset (fromIntegral fileSize), LineOffset currentLine)
                | otherwise = do
                    let bufSize = min fileChunkSize (fromIntegral fileSize - currentOffset)
                    bs <- BS.hGet h bufSize
                    if BS.null bs
                        then return (ByteOffset currentOffset, LineOffset currentLine)
                        else do
                            let newlineCount = BS.count 10 bs
                            if currentLine + newlineCount >= targetLine
                                then do
                                    let targetNewline = targetLine - currentLine
                                        indices = BS.elemIndices 10 bs
                                        position = case drop (targetNewline - 1) indices of
                                            (index : _) -> currentOffset + index + 1
                                            [] -> currentOffset
                                    return (ByteOffset position, LineOffset targetLine)
                                else go (currentOffset + BS.length bs) (currentLine + newlineCount)
        go 0 1

lineNumberAtOffset :: FilePath -> ByteOffset -> IO LineOffset
lineNumberAtOffset path (ByteOffset targetOffset)
    | targetOffset <= 0 = return (LineOffset 1)
    | otherwise = withBinaryFile path ReadMode $ \h -> do
        let go offset lineNumber
                | offset <= 0 = return (LineOffset lineNumber)
                | otherwise = do
                    let bufSize = min offset fileChunkSize
                        startRead = offset - bufSize
                    hSeek h AbsoluteSeek (fromIntegral startRead)
                    bs <- BS.hGet h bufSize
                    go startRead (lineNumber + BS.count 10 bs)
        go targetOffset 1

alignUTF8Start :: FilePath -> ByteOffset -> Integer -> IO ByteOffset
alignUTF8Start path offset@(ByteOffset requested) fileSize
    | requested <= 0 || fromIntegral requested >= fileSize = return offset
    | otherwise =
        withBinaryFile path ReadMode $ \h -> do
            hSeek h AbsoluteSeek (fromIntegral requested)
            prefix <- BS.hGet h 3
            return $ ByteOffset (requested + BS.length (BS.takeWhile (\byte -> byte >= 0x80 && byte < 0xC0) prefix))

readSeekChunk :: FilePath -> ByteOffset -> Int -> Integer -> Maybe LineOffset -> IO FileChunk
readSeekChunk path startOffset@(ByteOffset startValue) requestedBytes fileSize knownStartLine = do
    withBinaryFile path ReadMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral startValue)
        bs <- BS.hGet h (min fileChunkSize (max 0 requestedBytes))
        let trimmed = if startValue + BS.length bs < fromIntegral fileSize then trimToValidUTF8 bs else bs
            content' = TE.decodeUtf8With lenientDecode trimmed
            endOffset = ByteOffset (startValue + BS.length trimmed)
            eof' = startValue + BS.length trimmed >= fromIntegral fileSize
        resolvedStartLine@(LineOffset startLineValue) <- maybe (lineNumberAtOffset path startOffset) return knownStartLine
        let resolvedEndLine = LineOffset (startLineValue + BS.count 10 trimmed)
        return
            FileChunk
                { content = content'
                , startOffset = startOffset
                , endOffset = endOffset
                , startLine = resolvedStartLine
                , endLine = resolvedEndLine
                , eof = eof'
                }

-- | For valid UTF-8 input, the output is a prefix ending at a complete codepoint.
trimToValidUTF8 :: BS.ByteString -> BS.ByteString
trimToValidUTF8 bs
    | BS.null bs = bs
    | otherwise =
        let lastB = BS.last bs
         in case () of
                _
                    | lastB < 0x80 -> bs
                    | lastB >= 0xC0 -> BS.init bs
                    | otherwise ->
                        let (prefix, trail) = BS.spanEnd (\b -> b >= 0x80 && b < 0xC0) bs
                            trailLen = BS.length trail
                         in if BS.null prefix
                                then bs
                                else
                                    let startB = BS.last prefix
                                        needed = case () of
                                            _
                                                | startB >= 0xF0 -> 3
                                                | startB >= 0xE0 -> 2
                                                | startB >= 0xC0 -> 1
                                                | otherwise -> 0
                                     in if trailLen >= needed
                                            then bs
                                            else BS.take (BS.length prefix - 1) bs
