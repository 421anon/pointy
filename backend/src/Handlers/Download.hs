{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Download (
    discoverDownloadTemplates,
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
import Effectful (Eff, (:>))
import Effects (Eval)
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import UserRepo (RepoContext, runNixEvalJsonInRepo)


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


newtype PrefetchResult = PrefetchResult Text

instance FromJSON PrefetchResult where
    parseJSON = withObject "PrefetchResult" $ \o -> do
        h <- o .: "hash"
        when (T.null h) $ fail "Download produced empty hash"
        unless ("sha256-" `T.isPrefixOf` h) $
            fail $
                "Download hash missing sha256- prefix: " ++ T.unpack h
        return $ PrefetchResult h

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


discoverDownloadTemplates :: (RepoContext ctx, Eval :> es) => ctx -> ExceptT String (Eff es) (Set Text)
discoverDownloadTemplates ctx = do
    output <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
    case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack output))) of
        Left err -> throwError $ "Failed to decode stepConfig JSON: " ++ err
        Right (Object km) ->
            case KM.lookup "templates" km of
                Just (Object templates) ->
                    return $
                        Set.fromList
                            [ AK.toText key
                            | (key, Object tpl) <- KM.toList templates
                            , Just (String "download") <- [KM.lookup "kind" tpl]
                            ]
                _ -> throwError "stepConfig has no `templates` object"
        Right _ -> throwError "stepConfig is not a JSON object"


extractDownloadUrl :: Value -> Maybe Text
extractDownloadUrl val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    String url <- KM.lookup "url" args
    return url

extractDownloadHash :: Value -> Maybe Text
extractDownloadHash val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    Object downloaded <- KM.lookup "downloaded" args
    String h <- KM.lookup "hash" downloaded
    return h

extractDownloadedAt :: Value -> Maybe Text
extractDownloadedAt val = do
    Object obj <- Just val
    Object args <- KM.lookup "args" obj
    Object downloaded <- KM.lookup "downloaded" args
    String ts <- KM.lookup "downloadedAt" downloaded
    return ts

extractReqType :: Value -> Maybe Text
extractReqType (Object o) = case KM.lookup "type" o of
    Just (String t) -> Just t
    _ -> Nothing
extractReqType _ = Nothing

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
