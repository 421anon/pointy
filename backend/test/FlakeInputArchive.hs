{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import FlakeInputs (ensureFlakeInputs)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.Environment (getEnvironment, setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)

main :: IO ()
main = withSystemTempDirectory "flake-inputs" $ \root -> do
    let repo = root </> "repo"
        home = root </> "home"
        bin = root </> "bin"
        logPath = root </> "calls"
    mapM_ (createDirectoryIfMissing True) [bin, home]
    callProcess "git" ["init", "-q", repo]
    writeFake bin "nix" $ unlines
        [ "#!/bin/sh"
        , "printf 'nix %s\\n' \"$*\" >> \"$FAKE_LOG\""
        , "printf '%s\\n' '{\"inputs\":{\"dep\":{\"inputs\":{},\"path\":\"/nix/store/00000000000000000000000000000000-dep\"}},\"path\":\"/nix/store/11111111111111111111111111111111-source\"}'"
        ]
    writeFake bin "nix-store" $ unlines
        [ "#!/bin/sh"
        , "printf 'nix-store %s\\n' \"$*\" >> \"$FAKE_LOG\""
        , "ln -s \"$4\" \"$2\""
        ]
    environment <- getEnvironment
    setEnv "PATH" (bin ++ ":" ++ maybe "" id (lookup "PATH" environment))
    setEnv "HOME" home
    setEnv "FAKE_LOG" logPath
    writeFile (repo </> "flake.lock") "one\n"
    commit repo "one"
    first <- git repo ["rev-parse", "HEAD"]
    ensureFlakeInputs repo first
    calls <- readFile logPath
    assertEqual "one archive" 1 (countPrefix "nix " calls)
    assertEqual "one root" 1 (countPrefix "nix-store " calls)
    firstBlob <- git repo ["rev-parse", "HEAD:flake.lock"]
    let firstRoot = home </> ".local" </> "state" </> "pointy" </> "flake-inputs" </> firstBlob
    complete <- doesFileExist (firstRoot </> "complete")
    assertEqual "complete marker" True complete
    rooted <- pathIsSymbolicLink (firstRoot </> "00000000000000000000000000000000-dep")
    assertEqual "input root" True rooted
    ensureFlakeInputs repo first
    unchanged <- readFile logPath
    assertEqual "unchanged lock skips" 1 (countPrefix "nix " unchanged)
    writeFile (repo </> "flake.lock") "two\n"
    commit repo "two"
    second <- git repo ["rev-parse", "HEAD"]
    ensureFlakeInputs repo second
    secondCalls <- readFile logPath
    assertEqual "changed lock archives" 2 (countPrefix "nix " secondCalls)
    writeFile (repo </> "flake.lock") "three\n"
    commit repo "three"
    third <- git repo ["rev-parse", "HEAD"]
    ensureFlakeInputs repo third
    roots <- listDirectory (home </> ".local" </> "state" </> "pointy" </> "flake-inputs")
    assertEqual "keeps two lock roots" 2 (length (filter (/= "lock") roots))

writeFake :: FilePath -> FilePath -> String -> IO ()
writeFake bin name contents = do
    let path = bin </> name
    writeFile path contents
    callProcess "chmod" ["+x", path]

commit :: FilePath -> String -> IO ()
commit repo message = do
    callProcess "git" ["-C", repo, "add", "flake.lock"]
    callProcess "git" ["-C", repo, "-c", "user.name=test", "-c", "user.email=test@example.invalid", "commit", "-qm", message]

git :: FilePath -> [String] -> IO String
git repo args = trim <$> readProcess "git" ("-C" : repo : args) ""
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
    dropWhileEnd predicate = reverse . dropWhile predicate . reverse

countPrefix :: String -> String -> Int
countPrefix prefix = length . filter (prefix `isPrefixOf`) . lines

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (actual == expected) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
