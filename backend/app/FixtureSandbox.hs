{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Posix.Process (executeFile)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    case break (== "--") arguments of
        (_, []) -> exitFailure
        (_, _ : command : commandArguments) ->
            case command of
                "pi-glovebox-server" -> serve commandArguments
                _ -> executeFile command True commandArguments Nothing
        (_, ["--"]) -> exitFailure

serve :: [String] -> IO ()
serve (socketPath : _) = do
    createDirectoryIfMissing True (takeDirectory socketPath)
    writeFile socketPath ""
    forever (threadDelay 1000000)
serve [] = exitFailure
