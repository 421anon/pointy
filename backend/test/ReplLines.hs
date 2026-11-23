{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, bracket, try)
import Data.ByteString (ByteString, hPut)
import NixEvaluator.NixRepl (readLines)
import System.IO (IOMode (ReadMode, WriteMode), Handle, hClose, openBinaryFile)

main :: IO ()
main = do
    check
        "lines that share one read chunk all arrive"
        ["one", "two", "three", ""]
        =<< linesOf "one\ntwo\nthree\n\n"
    check
        "a last line without a newline is still reported once"
        ["one", "two"]
        =<< linesOf "one\ntwo"
    putStrLn "repl-lines: ok"

linesOf :: ByteString -> IO [ByteString]
linesOf contents =
    bracket (writeContents contents >> openRead) hClose $ \handle -> do
        collected <- newMVar []
        outcome <- try (readLines handle (\line -> modifyMVar_ collected (\lines_ -> pure (line : lines_)))) :: IO (Either SomeException ())
        case outcome of
            Left err -> fail (show err)
            Right () -> reverse <$> readMVar collected

writeContents :: ByteString -> IO ()
writeContents contents = bracket (openBinaryFile path WriteMode) hClose (`hPut` contents)

openRead :: IO Handle
openRead = openBinaryFile path ReadMode

path :: FilePath
path = "/tmp/pointy-repl-lines-test"

check :: String -> [ByteString] -> [ByteString] -> IO ()
check label expected actual
    | expected == actual = pure ()
    | otherwise = fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
