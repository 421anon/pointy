module ConfigWatch (watchConfig, replaceConfig, readConfigText) where

import Config (Config, parseConfig, readConfigFile, resolveConfigPath, warnMissingProviderKeys)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay)
import Control.Exception (IOException, displayException, try)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Directory (renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.INotify (Event (..), EventVariety (..), addWatch, initINotify)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import UserRepo (fetchRepo, syncUserRepo, withUserRepoExclusiveIO)

data Applied = Applied Int (Either String ())

{-# NOINLINE appliedVar #-}
appliedVar :: TVar Applied
appliedVar = unsafePerformIO $ newTVarIO (Applied 0 (Right ()))

watchConfig :: IO ()
watchConfig = do
    path <- resolveConfigPath
    inotify <- initINotify
    let watched = Just (rawPath (takeFileName path))
    void $
        addWatch inotify [CloseWrite, MoveIn] (rawPath (takeDirectory path)) $ \event ->
            when (changedFile event == watched) (applyConfig path)
  where
    rawPath = encodeUtf8 . T.pack

changedFile :: Event -> Maybe ByteString
changedFile event = case event of
    Closed{maybeFilePath = name} -> name
    MovedIn{filePath = name} -> Just name
    _ -> Nothing

applyConfig :: FilePath -> IO ()
applyConfig path = do
    putStrLn "Configuration changed, applying..."
    outcome <- runExceptT $ do
        config <- ExceptT (parseConfigFile path)
        liftIO (warnMissingProviderKeys config)
        ExceptT $ withUserRepoExclusiveIO (ExceptT (syncUserRepo config) >> fetchRepo)
    putStrLn $ either ("Warning: Failed to apply configuration: " ++) (const "Configuration applied.") outcome
    atomically $ modifyTVar' appliedVar (\(Applied count _) -> Applied (count + 1) outcome)

parseConfigFile :: FilePath -> IO (Either String Config)
parseConfigFile path = either unreadable parseConfig <$> try (readConfigFile path)
  where
    unreadable :: IOException -> Either String Config
    unreadable = Left . displayException

readConfigText :: IO Text
readConfigText = resolveConfigPath >>= readConfigFile

replaceConfig :: Text -> IO (Either String ())
replaceConfig contents = case parseConfig contents of
    Left err -> pure (Left err)
    Right _ -> do
        path <- resolveConfigPath
        let staged = path ++ ".staged"
        BS.writeFile staged (encodeUtf8 contents)
        setFileMode staged (unionFileModes ownerReadMode ownerWriteMode)
        Applied before _ <- readTVarIO appliedVar
        renameFile staged path
        awaitApplied before

awaitApplied :: Int -> IO (Either String ())
awaitApplied before = do
    expired <- registerDelay (600 * 1000000)
    atomically $ newOutcome `orElse` (timedOut <$ (readTVar expired >>= check))
  where
    newOutcome = do
        Applied count outcome <- readTVar appliedVar
        check (count > before)
        pure outcome
    timedOut = Left "The configuration was saved, but the backend did not apply it within 10 minutes."
