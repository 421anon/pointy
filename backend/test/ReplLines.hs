{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void)
import Data.ByteString (ByteString, hPut)
import NixEvaluator.NixRepl (readLines)
import System.IO (IOMode (ReadMode, WriteMode), Handle, hClose, openBinaryFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

main :: IO ()
main = do
    check
        "lines that share one read chunk all arrive"
        ["one", "two", "three", ""]
        =<< linesOfFile "one\ntwo\nthree\n\n"
    check
        "a last line without a newline is still reported once"
        ["one", "two"]
        =<< linesOfFile "one\ntwo"
    check
        "a line arrives before its stream has a full buffer"
        (Just "one")
        =<< firstLineOfSlowProcess
    putStrLn "repl-lines: ok"

linesOfFile :: ByteString -> IO [ByteString]
linesOfFile contents = bracket (writeContents contents >> openRead) hClose $ \handle -> do
    collected <- newMVar []
    readLines handle (\line -> modifyMVar_ collected (\lines_ -> pure (line : lines_)))
    reverse <$> readMVar collected

firstLineOfSlowProcess :: IO (Maybe ByteString)
firstLineOfSlowProcess =
    withCreateProcess ((proc "sh" ["-c", "printf 'one\\n'; sleep 5"]){std_out = CreatePipe}) $ \_ mStdout _ ph -> do
        case mStdout of
            Nothing -> fail "the test process has no stdout pipe"
            Just handle -> do
                first <- newEmptyMVar
                void $ forkIO $ void (try (readLines handle (\line -> putMVar first line)) :: IO (Either SomeException ()))
                result <- timeout 1000000 (takeMVar first)
                terminateProcess ph
                void (waitForProcess ph)
                pure result

writeContents :: ByteString -> IO ()
writeContents contents = bracket (openBinaryFile path WriteMode) hClose (`hPut` contents)

openRead :: IO Handle
openRead = openBinaryFile path ReadMode

path :: FilePath
path = "/tmp/pointy-repl-lines-test"

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual
    | expected == actual = pure ()
    | otherwise = fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
