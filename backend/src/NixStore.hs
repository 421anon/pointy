{-# LANGUAGE OverloadedStrings #-}

module NixStore (
    NixStore (..),
    nixStore,
    realStorePath,
) where

import Data.List (stripPrefix)
import Network.URI (unEscapeString)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

data NixStore = NixStore
    { storeRemote :: Maybe String
    , storeSocketPath :: Maybe FilePath
    , storeRoot :: Maybe FilePath
    }
    deriving (Eq, Show)

{-# NOINLINE nixStore #-}
nixStore :: NixStore
nixStore = unsafePerformIO $ maybe (pure hostStore) (pure . parseRemote) =<< lookupEnv "NIX_REMOTE"

hostStore :: NixStore
hostStore = NixStore Nothing Nothing Nothing

parseRemote :: String -> NixStore
parseRemote remote = case stripPrefix unixScheme remote of
    Nothing -> hostStore
    Just rest ->
        let (pathPart, queryPart) = break (== '?') rest
         in NixStore
                (Just remote)
                (absolutePath (unEscapeString pathPart))
                (unEscapeString <$> lookup "root" (queryParams (drop 1 queryPart)))
  where
    unixScheme = "unix://"

absolutePath :: FilePath -> Maybe FilePath
absolutePath path = case path of
    "" -> Nothing
    ('/' : _) -> Just path
    _ -> Just ('/' : path)

queryParams :: String -> [(String, String)]
queryParams = map pair . filter (not . null) . splitOn '&'
  where
    pair item =
        let (key, value) = break (== '=') item
         in (key, drop 1 value)
    splitOn separator text = case break (== separator) text of
        (chunk, []) -> [chunk]
        (chunk, _ : rest) -> chunk : splitOn separator rest

realStorePath :: FilePath -> FilePath
realStorePath path = case storeRoot nixStore of
    Nothing -> path
    Just root -> case stripPrefix logicalStoreDir path of
        Nothing -> path
        Just rest -> root </> "nix" </> "store" </> dropWhile (== '/') rest

logicalStoreDir :: FilePath
logicalStoreDir = "/nix/store"
