{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Autocomplete (AutocompleteRequest (..), autocompleteHandler) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Servant (Handler, throwError)
import Servant.Server (err400, err500, errBody)
import UserRepo (ReadRepoContext (..), fetchRepo, runNixEvalJsonApplyInRepo, withReadRepoTransaction)

data AutocompleteRequest = AutocompleteRequest
    { template :: Text
    , autocomplete :: Text
    , context :: Map Text Text
    , query :: Text
    , limit :: Maybe Int
    }
    deriving (Generic, Show)

instance FromJSON AutocompleteRequest

autocompleteHandler :: Maybe Text -> AutocompleteRequest -> Handler [Text]
autocompleteHandler mCommit req = do
    validateRequest req
    let clampedLimit = clampLimit (limit req)
        attr = buildAttr (template req) (autocomplete req)
        applyExpr = buildApplyExpr req clampedLimit
    result <- liftIO $ case mCommit of
        Just commit -> withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
            output <- runNixEvalJsonApplyInRepo (ReadRepoContext repoPath $ unpack commit) applyExpr attr
            decodeOutput output
        Nothing -> runExceptT $ do
            fetchRepo
            ExceptT $ withReadRepoTransaction $ \ctx -> do
                output <- runNixEvalJsonApplyInRepo ctx applyExpr attr
                decodeOutput output
    case result of
        Right values -> return values
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

validateRequest :: AutocompleteRequest -> Handler ()
validateRequest req = do
    requireSafeIdentifier "template" (template req)
    requireSafeIdentifier "autocomplete" (autocomplete req)
    requireSafePackageText "query" (query req)
    mapM_ validateContextEntry (Map.toList (context req))

validateContextEntry :: (Text, Text) -> Handler ()
validateContextEntry (key, value) = do
    requireSafeIdentifier ("context key " <> key) key
    requireSafePackageText ("context value " <> key) value

requireSafeIdentifier :: Text -> Text -> Handler ()
requireSafeIdentifier name value =
    unless (not (T.null value) && T.all isSafeIdentifierChar value) $
        throwError $
            err400{errBody = TLE.encodeUtf8 (TL.fromStrict $ name <> " contains unsafe characters")}

requireSafePackageText :: Text -> Text -> Handler ()
requireSafePackageText name value =
    unless (T.all isSafePackageChar value) $
        throwError $
            err400{errBody = TLE.encodeUtf8 (TL.fromStrict $ name <> " contains unsafe characters")}

isSafeIdentifierChar :: Char -> Bool
isSafeIdentifierChar c = isAsciiAlphaNum c || c == '_' || c == '-'

isSafePackageChar :: Char -> Bool
isSafePackageChar c = isAsciiAlphaNum c || c == '_' || c == '-' || c == '.' || c == '+'

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && (isAsciiLower c || isAsciiUpper c || isDigit c)

clampLimit :: Maybe Int -> Int
clampLimit Nothing = 25
clampLimit (Just n) = max 1 (min 100 n)

buildAttr :: Text -> Text -> String
buildAttr reqTemplate reqAutocomplete = "#pointy.autocomplete." <> unpack reqTemplate <> "." <> unpack reqAutocomplete

buildApplyExpr :: AutocompleteRequest -> Int -> String
buildApplyExpr req clampedLimit =
    "f: f { "
        <> concatMap renderAttr (Map.toList (context req))
        <> renderAttr ("query", query req)
        <> "limit = "
        <> show clampedLimit
        <> "; }"

renderAttr :: (Text, Text) -> String
renderAttr (key, value) = unpack key <> " = \"" <> unpack value <> "\"; "

decodeOutput :: String -> ExceptT String IO [Text]
decodeOutput output =
    ExceptT $
        return $
            case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
                Left err -> Left $ "decoding autocomplete output failed: " ++ err
                Right values -> Right values
