{-# LANGUAGE OverloadedStrings #-}

module Agent.TurnLog (
    appendLogLine,
    createTurnLog,
    readTurnLogFile,
) where

import Agent.TurnSignal (signalTurnLog)
import Config (AgentConfig (..))
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (IOException, bracket_, try)
import Control.Monad (when)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getFileSize)
import System.FilePath (normalise)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE lockedTurnLogs #-}
lockedTurnLogs :: TVar (Set FilePath)
lockedTurnLogs = unsafePerformIO (newTVarIO Set.empty)

-- GHC refuses to open a file that a handle in this process has open, unless both handles only read:
-- https://hackage.haskell.org/package/base/docs/System-IO.html#v:openFile
withTurnLogLock :: FilePath -> IO a -> IO a
withTurnLogLock path = bracket_ acquire release
  where
    key = normalise path
    acquire = atomically $ do
        locked <- readTVar lockedTurnLogs
        check (Set.notMember key locked)
        writeTVar lockedTurnLogs (Set.insert key locked)
    release = atomically $ modifyTVar' lockedTurnLogs (Set.delete key)

appendLogLine :: AgentConfig -> FilePath -> Text -> Text -> IO ()
appendLogLine cfg path label line = do
    appended <- withTurnLogLock path $ do
        size <- safeFileSize path
        let fits = size < fromIntegral (agentOutputLimitBytes cfg)
        when fits $ TIO.appendFile path ("[" <> label <> "] " <> line <> "\n")
        return fits
    when appended $ signalTurnLog path

createTurnLog :: FilePath -> Text -> IO ()
createTurnLog path = withTurnLogLock path . TIO.writeFile path

readTurnLogFile :: FilePath -> IO (Maybe Text)
readTurnLogFile path = withTurnLogLock path $ do
    exists <- doesFileExist path
    if exists then Just <$> TIO.readFile path else return Nothing

safeFileSize :: FilePath -> IO Integer
safeFileSize path = do
    result <- try (getFileSize path) :: IO (Either IOException Integer)
    case result of
        Left _ -> return 0
        Right size -> return size
