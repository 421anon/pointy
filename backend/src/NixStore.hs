{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module NixStore (
    NixStore (..),
    nixStore,
    rootedPath,
    resolveStorePath,
) where

import Control.Exception (IOException, try)
import Data.List (stripPrefix)
import Network.URI (unEscapeString)
import System.Directory (getSymbolicLinkTarget, pathIsSymbolicLink)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, joinPath, splitDirectories, (</>))
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

rootedPath :: FilePath -> FilePath
rootedPath path = case storeRoot nixStore of
    Nothing -> path
    Just root -> rootedUnder root path

rootedUnder :: FilePath -> FilePath -> FilePath
rootedUnder root path = case stripPrefix logicalNixDir path of
    Just rest@('/' : _) -> root </> "nix" </> dropWhile (== '/') rest
    _ -> path

resolveStorePath :: FilePath -> IO FilePath
resolveStorePath path = case storeRoot nixStore of
    Nothing -> pure path
    Just root -> resolveUnder root path

resolveUnder :: FilePath -> FilePath -> IO FilePath
resolveUnder root = walk maxSymlinks [] . splitDirectories
  where
    walk _ resolved [] = pure (rootedUnder root (logical resolved))
    walk budget resolved (component : rest) = case component of
        "/" -> walk budget [] rest
        "." -> walk budget resolved rest
        ".." -> walk budget (dropLast resolved) rest
        name -> do
            let candidate = resolved ++ [name]
            target <- symlinkTarget (rootedUnder root (logical candidate))
            case target of
                Nothing -> walk budget candidate rest
                Just _ | budget <= 0 -> ioError (userError ("too many levels of symbolic links: " ++ logical candidate))
                Just link
                    | isAbsolute link -> walk (budget - 1) [] (splitDirectories link ++ rest)
                    | otherwise -> walk (budget - 1) resolved (splitDirectories link ++ rest)
    logical = joinPath . ("/" :)
    dropLast = reverse . drop 1 . reverse

symlinkTarget :: FilePath -> IO (Maybe FilePath)
symlinkTarget path = do
    isLink <- try @IOException (pathIsSymbolicLink path)
    case isLink of
        Right True -> Just <$> getSymbolicLinkTarget path
        _ -> pure Nothing

maxSymlinks :: Int
maxSymlinks = 40

logicalNixDir :: FilePath
logicalNixDir = "/nix"
