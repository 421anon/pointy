{-# LANGUAGE OverloadedStrings #-}

module Fixture.Server (fixtureApp) where

import App (stripBackendPrefix)
import Control.Exception (SomeException, try)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Interpreters.Fixture (FixtureState, resetFixture)
import Data.List (intercalate)
import Network.HTTP.Types (Status, hContentType, status200, status404, status405, status500)
import Network.Wai (Application, Request (..), Response, pathInfo, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))

fixtureApp :: FixtureState -> FilePath -> IO () -> Application -> Application
fixtureApp state frontendDir reset backend request respond =
    case (requestMethod request, pathInfo request) of
        ("POST", ["fixture", "reset"]) -> do
            outcome <- try reset
            case outcome of
                Left err -> respond (json status500 (object ["error" .= show (err :: SomeException)]))
                Right () -> respond (json status200 (object ["reset" .= True]))
        (_, ["fixture", "health"]) -> respond (json status200 (object ["ready" .= True]))
        (_, ("backend" : _)) -> stripBackendPrefix backend request respond
        ("GET", _) -> serveFrontend frontendDir request respond
        _ -> respond (responseLBS status405 [(hContentType, "text/plain")] "method not allowed")

json :: Status -> Value -> Response
json status value = responseLBS status [(hContentType, "application/json")] (encode value)

serveFrontend :: FilePath -> Application
serveFrontend frontendDir request respond =
    case safeRelativePath (pathInfo request) of
        Nothing -> respond notFound
        Just relative -> do
            served <- respondFile frontendDir relative respond
            case served of
                Just received -> pure received
                Nothing ->
                    if hasExtension relative
                        then respond notFound
                        else do
                            fallback <- respondFile frontendDir "index.html" respond
                            maybe (respond notFound) pure fallback

respondFile :: FilePath -> FilePath -> (Response -> IO a) -> IO (Maybe a)
respondFile root relative respond = do
    let path = if null relative then root else root </> relative
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            body <- LBS.readFile path
            received <- respond (responseLBS status200 [(hContentType, TE.encodeUtf8 (mimeType relative))] body)
            pure (Just received)

mimeType :: FilePath -> Text
mimeType path = case extension of
    "html" -> "text/html; charset=utf-8"
    "js" -> "text/javascript; charset=utf-8"
    "mjs" -> "text/javascript; charset=utf-8"
    "css" -> "text/css; charset=utf-8"
    "json" -> "application/json"
    "svg" -> "image/svg+xml"
    "png" -> "image/png"
    "jpg" -> "image/jpeg"
    "jpeg" -> "image/jpeg"
    "gif" -> "image/gif"
    "ico" -> "image/x-icon"
    "woff" -> "font/woff"
    "woff2" -> "font/woff2"
    "ttf" -> "font/ttf"
    "map" -> "application/json"
    "wasm" -> "application/wasm"
    "txt" -> "text/plain; charset=utf-8"
    _ -> "application/octet-stream"
  where
    extension = drop 1 (takeExtension path)

safeRelativePath :: [Text] -> Maybe FilePath
safeRelativePath segments
    | any unsafe segments = Nothing
    | otherwise = Just (intercalate "/" (map T.unpack segments))
  where
    unsafe segment = T.null segment || T.isInfixOf ".." segment || T.any (== '\0') segment

hasExtension :: FilePath -> Bool
hasExtension path = not (null (takeExtension path))

notFound :: Response
notFound = responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "not found"
