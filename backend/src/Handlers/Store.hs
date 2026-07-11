{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handlers.Store (listHandler, downloadHandler, seekHandler, storeFilesHandler, stepListHandler, stepDownloadHandler, stepSeekHandler, stepRawHandler, stepExtrasHandler, DirEntry (..), FileChunk (..), SeekPosition (..), fileChunkSize, maxViewableSize, checkViewableAndMime, parseSeekPosition, seekFileChunk', trimToValidUTF8) where

import ApiTypes (DynamicJson (..))

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, void, when)
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
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Generics (Generic)
import Handlers.RunStep (buildExtras)
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
fileChunkSize = 262144 -- 256 KiB

maxViewableSize :: Integer
maxViewableSize = 5 * 1024 * 1024 -- 5 MiB

readFileChunked :: FilePath -> S.SourceT IO BS.ByteString
readFileChunked path = S.SourceT $ \k ->
    withBinaryFile path ReadMode $ \h ->
        k $ readChunks h
  where
    readChunks h = S.fromActionStep BS.null (BS.hGet h fileChunkSize)

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

-- | Bounded chunk of a file returned by the seek API.
data FileChunk = FileChunk
    { content :: Text
    , startOffset :: Int
    , endOffset :: Int
    , startLine :: Int
    , endLine :: Int
    , eof :: Bool
    }
    deriving (Generic, Show, ToJSON)

-- | Typed seek position from user-facing query parameters.
data SeekPosition
    = FromStart
    | AtLine Int
    | AtOffset Int
    | BeforeOffset Int
    deriving (Show, Eq)

-- | Parse and validate the three Maybe Int query params into a SeekPosition.
parseSeekPosition :: Maybe Int -> Maybe Int -> Maybe Int -> Handler SeekPosition
parseSeekPosition mLine mOffset mBefore = do
    when (length (filter (/= Nothing) [mLine, mOffset, mBefore]) > 1) $
        throwError err400{errBody = "Specify only one of line, offset, or before"}
    case (mLine, mOffset, mBefore) of
        (Just l, _, _)
            | l < 1 -> throwError err400{errBody = "line must be >= 1"}
            | otherwise -> return $ AtLine l
        (_, Just o, _)
            | o < 0 -> throwError err400{errBody = "offset must be >= 0"}
            | otherwise -> return $ AtOffset o
        (_, _, Just o)
            | o < 0 -> throwError err400{errBody = "before must be >= 0"}
            | otherwise -> return $ BeforeOffset o
        (Nothing, Nothing, Nothing) -> return FromStart

{- | GET /step-files/seek?id=<stepId>&commit=<commit>&path=<filePath>[&line=<N>|&offset=<N>|&before=<N>]
Returns a bounded chunk beginning at a line/offset, or ending before an offset.
-}
stepSeekHandler :: Int -> Maybe Text -> FilePath -> Maybe Int -> Maybe Int -> Maybe Int -> Handler FileChunk
stepSeekHandler stepId mCommit rel mLine mOffset mBefore = do
    pos <- parseSeekPosition mLine mOffset mBefore
    outPath <- resolveStepOutPath stepId mCommit
    seekHandler outPath rel pos

seekHandler :: Text -> FilePath -> SeekPosition -> Handler FileChunk
seekHandler basePathText rel pos = do
    let basePath = T.unpack basePathText
    assertNixStorePath basePath
    let absPath = normalise (basePath </> rel)
    assertInside absPath basePath
    isFile <- liftIO $ doesFileExist absPath
    unless isFile $ throwError err404
    fileSize <- liftIO $ getFileSize absPath
    let beyondEnd = case pos of
            AtOffset o | fromIntegral o > fileSize -> True
            BeforeOffset o | fromIntegral o > fileSize -> True
            _ -> False
    when beyondEnd $
        throwError err400{errBody = "seek position beyond end of file"}
    liftIO $ seekFileChunk' absPath pos fileSize

-- | Core seek logic: read a bounded chunk at a typed position.
seekFileChunk' :: FilePath -> SeekPosition -> Integer -> IO FileChunk
seekFileChunk' path pos fileSize = do
    requestedOffset <- case pos of
        FromStart -> return 0
        AtLine l -> findLineOffset path l fileSize
        AtOffset o -> return o
        BeforeOffset before -> return (max 0 (before - fileChunkSize))
    startOff <- alignUTF8Start path requestedOffset fileSize
    let requestedBytes = case pos of
            BeforeOffset before -> before - startOff
            _ -> fileChunkSize
    readSeekChunk path startOff requestedBytes fileSize

-- | Scan forward in fixed-size blocks to locate the byte offset where targetLine begins.
findLineOffset :: FilePath -> Int -> Integer -> IO Int
findLineOffset path targetLine fileSize =
    withBinaryFile path ReadMode $ \h -> do
        let go currentOffset currentLine
                | currentLine >= targetLine = return currentOffset
                | currentOffset >= fromIntegral fileSize = return (fromIntegral fileSize)
                | otherwise = do
                    let bufSize = min fileChunkSize (fromIntegral fileSize - currentOffset)
                    bs <- BS.hGet h bufSize
                    if BS.null bs
                        then return currentOffset
                        else do
                            let nlCount = BS.count 10 bs
                            if currentLine + nlCount >= targetLine
                                then do
                                    let targetNl = targetLine - currentLine
                                        indices = BS.elemIndices 10 bs
                                        pos = case drop (targetNl - 1) indices of
                                            (i : _) -> currentOffset + i + 1
                                            [] -> currentOffset
                                    return pos
                                else go (currentOffset + BS.length bs) (currentLine + nlCount)
        go 0 1

-- | Count newlines in fixed-size blocks to determine the 1-based line number at a given byte offset.
countPrecedingNewlines :: FilePath -> Int -> IO Int
countPrecedingNewlines path upTo
    | upTo <= 0 = return 1
    | otherwise = withBinaryFile path ReadMode $ \h -> do
        let go offset lineCount
                | offset <= 0 = return lineCount
                | otherwise = do
                    let bufSize = min offset fileChunkSize
                        startRead = offset - bufSize
                    hSeek h AbsoluteSeek (fromIntegral startRead)
                    bs <- BS.hGet h bufSize
                    let nlCount = BS.count 10 bs
                    go startRead (lineCount + nlCount)
        go upTo 1

-- | Move an arbitrary byte offset past UTF-8 continuation bytes.
alignUTF8Start :: FilePath -> Int -> Integer -> IO Int
alignUTF8Start path requested fileSize
    | requested <= 0 || fromIntegral requested >= fileSize = return requested
    | otherwise =
        withBinaryFile path ReadMode $ \h -> do
            hSeek h AbsoluteSeek (fromIntegral requested)
            prefix <- BS.hGet h 3
            return $ requested + BS.length (BS.takeWhile (\b -> b >= 0x80 && b < 0xC0) prefix)

-- | Read a bounded number of bytes from startOff and compute line metadata.
readSeekChunk :: FilePath -> Int -> Int -> Integer -> IO FileChunk
readSeekChunk path startOff requestedBytes fileSize = do
    withBinaryFile path ReadMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral startOff)
        bs <- BS.hGet h (min fileChunkSize (max 0 requestedBytes))
        let trimmed = if startOff + BS.length bs < fromIntegral fileSize then trimToValidUTF8 bs else bs
            content' = TE.decodeUtf8With lenientDecode trimmed
            endOff = startOff + BS.length trimmed
            eof' = endOff >= fromIntegral fileSize
        startLine' <- countPrecedingNewlines path startOff
        let endLine' = startLine' + BS.count 10 trimmed
        return
            FileChunk
                { content = content'
                , startOffset = startOff
                , endOffset = endOff
                , startLine = startLine'
                , endLine = endLine'
                , eof = eof'
                }

{- | Trim a ByteString so it ends on a valid UTF-8 sequence boundary.
When the input is valid UTF-8, the output is a prefix ending at a complete codepoint.
-}
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
