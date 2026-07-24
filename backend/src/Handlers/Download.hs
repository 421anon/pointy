{-# LANGUAGE OverloadedStrings #-}

{- | Download-kind step support: URL prefetch and hash injection.

The browser sends @args.url@ as a top-level template option.  The backend
prefetches the URL, then injects @args.downloaded = { url = args.url; hash; downloadedAt? }@
so the existing stdlib resolver works unchanged.

Download-step detection uses @#pointy.stepConfig.\<template\>.type.download@
rather than any hard-coded template name.
-}
module Handlers.Download (
    discoverDownloadTemplates,
    downloadTemplatesFromConfig,
    loadStepConfig,
    prefetchFile,
    extractDownloadUrl,
    extractDownloadHash,
    extractDownloadedAt,
    extractReqType,
    injectDownloaded,
    validateHttpUrl,
)
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, withObject, (.:))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LB
import Data.Char (isSpace, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import UserRepo (RepoContext, runNixEvalJsonInRepo)

-----------------------------------------------------------------------------
-- URL validation
-----------------------------------------------------------------------------

{- | Validate that a URL uses the @http@ or @https@ scheme, is non-empty,
has a non-empty host, and contains no whitespace.  Error messages never
echo the potentially untrusted URL value.
-}
validateHttpUrl :: Text -> Either String Text
validateHttpUrl url
    | T.null url = Left "Download URL must not be empty"
    | T.any isSpace url = Left "Download URL must not contain whitespace"
    | otherwise = case parseURI (T.unpack url) of
        Nothing -> Left "Download URL must use http or https scheme"
        Just uri ->
            let scheme = map toLower (uriScheme uri)
             in if scheme /= "http:" && scheme /= "https:"
                    then Left "Download URL must use http or https scheme"
                    else case uriAuthority uri of
                        Nothing -> Left "Download URL has empty host"
                        Just auth
                            | null (uriRegName auth) -> Left "Download URL has empty host"
                            | otherwise -> Right url

-----------------------------------------------------------------------------
-- nix store prefetch-file
-----------------------------------------------------------------------------

newtype PrefetchResult = PrefetchResult Text

instance FromJSON PrefetchResult where
    parseJSON = withObject "PrefetchResult" $ \o -> do
        h <- o .: "hash"
        when (T.null h) $ fail "Download produced empty hash"
        unless ("sha256-" `T.isPrefixOf` h) $
            fail $
                "Download hash missing sha256- prefix: " ++ T.unpack h
        return $ PrefetchResult h

{- | Run @nix store prefetch-file --json --hash-type sha256 \<url\>@ using
process arguments (never a shell).  Returns the validated hash and the
current UTC time formatted as RFC 3339 on success.
-}
prefetchFile :: Text -> IO (Either String (Text, Text))
prefetchFile url = do
    (exitCode, stdout', stderr) <-
        readProcessWithExitCode
            "nix"
            ["store", "prefetch-file", "--json", "--hash-type", "sha256", T.unpack url]
            ""
    case exitCode of
        ExitFailure code -> do
            putStrLn $ "download command failed with exit code " ++ show code ++ ": " ++ stderr
            return $ Left "Download failed"
        ExitSuccess ->
            case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack stdout'))) of
                Left err ->
                    return $ Left $ "Failed to parse download result: " ++ err
                Right (PrefetchResult h) -> do
                    now <- getCurrentTime
                    let ts = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
                    return $ Right (h, ts)

-----------------------------------------------------------------------------
-- Step-config classification
-----------------------------------------------------------------------------

loadStepConfig :: (RepoContext ctx) => ctx -> ExceptT String IO Value
loadStepConfig ctx = do
    output <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
    case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack output))) of
        Left err -> throwError $ "Failed to decode stepConfig JSON: " ++ err
        Right value -> return value

downloadTemplatesFromConfig :: Value -> Either String (Set Text)
downloadTemplatesFromConfig (Object config) =
    Right $
        Set.fromList
            [ AK.toText key
            | (key, value) <- KM.toList config
            , hasDownloadType value
            ]
  where
    hasDownloadType (Object entry) =
        case KM.lookup "type" entry of
            Just (Object typeObject) -> KM.member "download" typeObject
            _ -> False
    hasDownloadType _ = False
downloadTemplatesFromConfig _ = Left "stepConfig is not a JSON object"

{- | Evaluate @#pointy.stepConfig@ and return the set of template names whose
@type.download@ attribute is present (i.e. the step kind is \"download\").

Fails with an error when the evaluated JSON is not an object.
-}
discoverDownloadTemplates :: (RepoContext ctx) => ctx -> ExceptT String IO (Set Text)
discoverDownloadTemplates ctx = do
    config <- loadStepConfig ctx
    either throwError return (downloadTemplatesFromConfig config)

-----------------------------------------------------------------------------
-- JSON navigation helpers
-----------------------------------------------------------------------------

{- | Extract the top-level @args.url@ from a step JSON value (the field the
browser sends).  This is the canonical download URL; the backend copies it
into @args.downloaded.url@ alongside the prefetched hash.
-}
extractDownloadUrl :: Value -> Maybe Text
extractDownloadUrl val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    String url <- KM.lookup "url" args
    return url

-- | Extract @args.downloaded.hash@ from a step JSON value.
extractDownloadHash :: Value -> Maybe Text
extractDownloadHash val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    Object downloaded <- KM.lookup "downloaded" args
    String h <- KM.lookup "hash" downloaded
    return h

{- | Extract @args.downloaded.downloadedAt@ from a step JSON value.
Returns 'Nothing' for legacy records that lack the timestamp field.
-}
extractDownloadedAt :: Value -> Maybe Text
extractDownloadedAt val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    Object downloaded <- KM.lookup "downloaded" args
    String ts <- KM.lookup "downloadedAt" downloaded
    return ts

-- | Extract the @type@ field from a step JSON value.
extractReqType :: Value -> Maybe Text
extractReqType (Object o) = case KM.lookup "type" o of
    Just (String t) -> Just t
    _ -> Nothing
extractReqType _ = Nothing

{- | Inject trusted download provenance by reading the canonical @args.url@ and
constructing @args.downloaded = { url = args.url; hash; downloadedAt? }@.

Any pre-existing @args.downloaded@ from the client is replaced entirely
so that client-supplied hash values, timestamps, or extra fields are
never preserved.  @downloadedAt@ is omitted when 'Nothing' (legacy
records) and formatted as UTC RFC 3339 when present.
-}
injectDownloaded :: Value -> Text -> Maybe Text -> Value
injectDownloaded val hash mTs = case val of
    Object obj ->
        let argsKey = AK.fromText "args"
            urlKey = AK.fromText "url"
            downloadedKey = AK.fromText "downloaded"
            hashKey = AK.fromText "hash"
            atKey = AK.fromText "downloadedAt"
            tsField = case mTs of
                Just ts -> [(atKey, String ts)]
                Nothing -> []
         in case KM.lookup argsKey obj of
                Just (Object args) ->
                    case KM.lookup urlKey args of
                        Just urlVal ->
                            let newDownloaded =
                                    KM.fromList $
                                        [(urlKey, urlVal), (hashKey, String hash)] ++ tsField
                                newArgs = KM.insert downloadedKey (Object newDownloaded) args
                             in Object (KM.insert argsKey (Object newArgs) obj)
                        _ -> val
                _ -> val
    _ -> val
