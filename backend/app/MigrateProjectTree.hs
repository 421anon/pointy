{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import ProjectFiles (jsonToNix)
import Processes (cli)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Read (readMaybe)

main :: IO ()
main = do
    setLocaleEncoding utf8
    arguments <- getArgs
    case arguments of
        [worktree] -> migrate worktree
        _ -> die "usage: pointy-migrate-project-tree <user-repo-worktree>"

migrate :: FilePath -> IO ()
migrate worktree = do
    projectsDir <- makeAbsolute (worktree </> "projects")
    let rootPath = projectsDir </> "0.nix"
    hasProjects <- doesDirectoryExist projectsDir
    unless hasProjects $ die (projectsDir ++ " does not exist.")
    migrated <- doesFileExist rootPath
    when migrated $ die (rootPath ++ " already exists; the projects are already in the children format.")
    projectIds <- sort . mapMaybe projectFileId <$> listDirectory projectsDir
    projects <- forM projectIds $ \projectId -> (,) projectId <$> readProject (projectFile projectsDir projectId)
    converted <- either die pure (traverse (\(projectId, project) -> (,) projectId <$> withChildren projectId project) projects)
    forM_ converted $ \(projectId, project) -> writeNix (projectFile projectsDir projectId) project
    writeNix rootPath (rootProject projects)
    putStrLn ("Moved the steps of " ++ show (length projects) ++ " project files into children and wrote " ++ rootPath ++ ".")

projectFileId :: FilePath -> Maybe Int
projectFileId file
    | takeExtension file == ".nix" = readMaybe (takeBaseName file)
    | otherwise = Nothing

projectFile :: FilePath -> Int -> FilePath
projectFile projectsDir projectId = projectsDir </> show projectId ++ ".nix"

readProject :: FilePath -> IO Value
readProject path = do
    (code, output, errors) <- cli "nix" ["--extra-experimental-features", "nix-command", "eval", "--impure", "--json", "--expr", "import " ++ path]
    case code of
        ExitFailure _ -> die ("Failed to evaluate " ++ path ++ ":\n" ++ errors)
        ExitSuccess -> either (\err -> die ("Failed to decode " ++ path ++ ": " ++ err)) pure (eitherDecode (TLE.encodeUtf8 (TL.pack output)))

withChildren :: Int -> Value -> Either String Value
withChildren projectId (Object fields) = case KeyMap.lookup "steps" fields of
    Just (Array steps) -> Right (Object (KeyMap.insert "children" (Array (V.map (\step -> object ["step" .= step]) steps)) (foldr KeyMap.delete fields ["steps", "hidden", "sortKey"])))
    _ -> Left ("projects/" ++ show projectId ++ ".nix has no steps list.")
withChildren projectId _ = Left ("projects/" ++ show projectId ++ ".nix is not an attribute set.")

rootProject :: [(Int, Value)] -> Value
rootProject projects =
    object
        [ "children" .= [object ["project" .= object ["hidden" .= placement "hidden" (Bool False) project, "id" .= projectId, "sortKey" .= placement "sortKey" Null project]] | (projectId, project) <- projects]
        , "name" .= ("Home" :: String)
        , "templates" .= ([] :: [String])
        ]
  where
    placement key fallback (Object fields) = fromMaybe fallback (KeyMap.lookup key fields)
    placement _ fallback _ = fallback

writeNix :: FilePath -> Value -> IO ()
writeNix path value = either (\err -> die ("Failed to render " ++ path ++ ": " ++ err)) (\nix -> TIO.writeFile path (nix <> "\n")) (jsonToNix (encode value))
