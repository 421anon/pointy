module ProcessLimiter (
    readProcessWithExitCodeL,
    readCreateProcessWithExitCodeL,
) where

import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import System.Exit (ExitCode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess, readCreateProcessWithExitCode, readProcessWithExitCode)

{-# NOINLINE processSem #-}
processSem :: QSem
processSem = unsafePerformIO (newQSem 40) -- Allow max 40 concurrent processes

withProcessLimit :: IO a -> IO a
withProcessLimit = bracket_ (waitQSem processSem) (signalQSem processSem)

readProcessWithExitCodeL :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCodeL cmd args inp = withProcessLimit $ readProcessWithExitCode cmd args inp

readCreateProcessWithExitCodeL :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCodeL cp inp = withProcessLimit $ readCreateProcessWithExitCode cp inp
