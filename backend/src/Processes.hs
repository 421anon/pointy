{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Processes (cli, cliBytes) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)

cli :: FilePath -> [String] -> IO (ExitCode, String, String)
cli command args = do
    (code, stdout, stderr) <- cliBytes command args
    return (code, T.unpack (TE.decodeUtf8Lenient stdout), T.unpack (TE.decodeUtf8Lenient stderr))

cliBytes :: FilePath -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
cliBytes command args = do
    outcome <- try @IOException $ do
        withCreateProcess (proc command args){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe} $ \mIn mOut mErr processHandle -> do
            mapM_ hClose mIn
            (stdout, stderr) <- concurrently (drainHandle mOut) (drainHandle mErr)
            code <- waitForProcess processHandle
            return (code, stdout, stderr)
    return $ case outcome of
        Left err -> (ExitFailure 127, BS.empty, TE.encodeUtf8 (T.pack (show err)))
        Right result -> result

drainHandle :: Maybe Handle -> IO BS.ByteString
drainHandle Nothing = return BS.empty
drainHandle (Just handle_) = BS.hGetContents handle_
