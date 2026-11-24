{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FlakeInputs (ensureFlakeInputs) where

import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Processes (cliBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, getModificationTime, listDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (takeFileName, (</>))
import System.Timeout (timeout)
import UserRepo (runGitIn)

ensureFlakeInputs :: FilePath -> String -> IO ()
ensureFlakeInputs repoPath commit = do
    outcome <- try @SomeException $ do
        lockHash <- flakeLockBlob repoPath commit
        forM_ lockHash $ \hash -> do
            stateDir <- flakeInputStateDir
            createDirectoryIfMissing True stateDir
            withFileLock (stateDir </> "lock") Exclusive $ \_ -> do
                let rootDir = stateDir </> hash
                complete <- doesFileExist (rootDir </> "complete")
                unless complete $ do
                    result <- archiveFlakeInputs repoPath commit rootDir
                    case result of
                        Left err -> putStrLn ("Warning: could not archive flake inputs: " ++ err)
                        Right () -> do
                            writeFile (rootDir </> "complete") ""
                            pruneFlakeInputRoots stateDir hash
    case outcome of
        Left err -> putStrLn ("Warning: could not archive flake inputs: " ++ displayException err)
        Right () -> pure ()

flakeInputStateDir :: IO FilePath
flakeInputStateDir = (</> ".local" </> "state" </> "pointy" </> "flake-inputs") <$> getHomeDirectory

flakeLockBlob :: FilePath -> String -> IO (Maybe String)
flakeLockBlob repoPath commit = do
    (code, stdout, _) <- runGitIn repoPath ["rev-parse", commit ++ ":flake.lock"]
    pure $ case code of
        ExitSuccess -> Just (takeWhile (/= '\n') stdout)
        ExitFailure _ -> Nothing

archiveFlakeInputs :: FilePath -> String -> FilePath -> IO (Either String ())
archiveFlakeInputs repoPath commit rootDir = do
    let installable = "git+file://" ++ repoPath ++ "?rev=" ++ commit
    result <- timeout archiveTimeout (cliBytes "nix" ["flake", "archive", "--json", installable])
    case result of
        Nothing -> pure (Left "nix flake archive timed out")
        Just (code, stdout, stderr) -> case code of
            ExitFailure _ -> pure (Left (T.unpack (TE.decodeUtf8Lenient stderr)))
            ExitSuccess -> case Aeson.eitherDecode (LBS.fromStrict stdout) of
                Left parseErr -> pure (Left ("could not parse nix flake archive output: " ++ parseErr))
                Right value -> addRoots rootDir (archiveInputPaths value)
  where
    archiveTimeout = 300 * 1000000

archiveInputPaths :: Aeson.Value -> [FilePath]
archiveInputPaths value = case value of
    Aeson.Object root -> case KeyMap.lookup "inputs" root of
        Just (Aeson.Object inputs) -> nub (concatMap collect (KeyMap.elems inputs))
        _ -> []
    _ -> []
  where
    collect (Aeson.Object object) =
        pathOf (KeyMap.lookup "path" object)
            ++ case KeyMap.lookup "inputs" object of
                Just (Aeson.Object inputs) -> concatMap collect (KeyMap.elems inputs)
                _ -> []
    collect _ = []
    pathOf (Just (Aeson.String path)) = [T.unpack path]
    pathOf _ = []

addRoots :: FilePath -> [FilePath] -> IO (Either String ())
addRoots rootDir paths = do
    createDirectoryIfMissing True rootDir
    results <- forM paths $ \path -> do
        let root = rootDir </> takeFileName path
        void (try @IOException (removeFile root))
        (code, _, stderr) <- cliBytes "nix-store" ["--add-root", root, "--realise", path]
        pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure _ -> Left (T.unpack (TE.decodeUtf8Lenient stderr))
    pure $ case [err | Left err <- results] of
        [] -> Right ()
        err : _ -> Left err

pruneFlakeInputRoots :: FilePath -> String -> IO ()
pruneFlakeInputRoots stateDir current = do
    entries <- listDirectory stateDir
    completeRoots <- fmap catMaybes $ forM entries $ \name -> do
        let path = stateDir </> name
        isDirectory <- doesDirectoryExist path
        complete <- doesFileExist (path </> "complete")
        if isDirectory && name /= current && complete
            then do
                modified <- getModificationTime path
                pure (Just (modified, path))
            else pure Nothing
    let keep = map snd (take 1 (sortOn (Down . fst) completeRoots))
    forM_ entries $ \name -> do
        let path = stateDir </> name
        isDirectory <- doesDirectoryExist path
        when (isDirectory && name /= current && path `notElem` keep) $
            void (try @IOException (removePathForcibly path))
