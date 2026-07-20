{-# LANGUAGE OverloadedStrings #-}

{- | Download-kind step support: URL prefetch and hash injection.

The browser sends @args.url@ as a top-level template option.  The backend
prefetches the URL, then injects @args.downloaded = { url = args.url; hash }@
so the existing stdlib resolver works unchanged.

Download-step detection uses @#pointy.stepConfig.\<template\>.type.download@
rather than any hard-coded template name.
-}
module Handlers.Download (
    discoverDownloadTemplates,
    prefetchFile,
    extractDownloadUrl,
    extractDownloadHash,
    extractReqType,
    injectDownloadHash,
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
        when (T.null h) $ fail "prefetch-file returned empty hash"
        unless ("sha256-" `T.isPrefixOf` h) $
            fail $
                "prefetch-file hash missing sha256- prefix: " ++ T.unpack h
        return $ PrefetchResult h

{- | Run @nix store prefetch-file --json --hash-type sha256 \<url\>@ using
process arguments (never a shell).  Returns the validated hash on success.
-}
prefetchFile :: Text -> IO (Either String Text)
prefetchFile url = do
    (exitCode, stdout', stderr) <-
        readProcessWithExitCode
            "nix"
            ["store", "prefetch-file", "--json", "--hash-type", "sha256", T.unpack url]
            ""
    case exitCode of
        ExitFailure _ ->
            return $ Left $ "nix store prefetch-file failed: " ++ stderr
        ExitSuccess ->
            case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack stdout'))) of
                Left err ->
                    return $ Left $ "Failed to parse prefetch-file JSON output: " ++ err
                Right (PrefetchResult h) ->
                    return $ Right h

-----------------------------------------------------------------------------
-- Step-config classification
-----------------------------------------------------------------------------

{- | Evaluate @#pointy.stepConfig@ and return the set of template names whose
@type.download@ attribute is present (i.e. the step kind is \"download\").

Fails with an error when the evaluated JSON is not an object.
-}
discoverDownloadTemplates :: (RepoContext ctx) => ctx -> ExceptT String IO (Set Text)
discoverDownloadTemplates ctx = do
    output <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
    case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack output))) of
        Left err -> throwError $ "Failed to decode stepConfig JSON: " ++ err
        Right (Object km) ->
            return $
                Set.fromList
                    [ AK.toText key
                    | (key, val) <- KM.toList km
                    , hasDownloadType val
                    ]
        Right _ -> throwError "stepConfig is not a JSON object"
  where
    hasDownloadType :: Value -> Bool
    hasDownloadType (Object obj) =
        case KM.lookup "type" obj of
            Just (Object typeObj) -> KM.member "download" typeObj
            _ -> False
    hasDownloadType _ = False

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

-- | Extract the @type@ field from a step JSON value.
extractReqType :: Value -> Maybe Text
extractReqType (Object o) = case KM.lookup "type" o of
    Just (String t) -> Just t
    _ -> Nothing
extractReqType _ = Nothing

{- | Inject a trusted hash by reading the canonical @args.url@ and
constructing @args.downloaded = { url = args.url; hash }@.

Any pre-existing @args.downloaded@ from the client is replaced entirely
so that client-supplied hash values or extra fields are never preserved.
-}
injectDownloadHash :: Value -> Text -> Value
injectDownloadHash val hash = case val of
    Object obj ->
        let argsKey = AK.fromText "args"
            urlKey = AK.fromText "url"
            downloadedKey = AK.fromText "downloaded"
            hashKey = AK.fromText "hash"
         in case KM.lookup argsKey obj of
                Just (Object args) ->
                    case KM.lookup urlKey args of
                        Just urlVal ->
                            let newDownloaded = KM.fromList [(urlKey, urlVal), (hashKey, String hash)]
                                newArgs = KM.insert downloadedKey (Object newDownloaded) args
                             in Object (KM.insert argsKey (Object newArgs) obj)
                        _ -> val
                _ -> val
    _ -> val
