{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Processes (cli) where

import Control.Exception (IOException, try)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

cli :: FilePath -> [String] -> IO (ExitCode, String, String)
cli command args = do
    outcome <- try @IOException (readProcessWithExitCode command args "")
    pure $ case outcome of
        Left err -> (ExitFailure 127, "", show err)
        Right result -> result
