{-# LANGUAGE ScopedTypeVariables #-}

module ConfigWatch (watchConfig, modifyConfig, readConfigText) where

import Config (Config, parseConfig, readConfigFile, resolveConfigPath, warnMissingProviderKeys)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (ErrorCall, Handler (..), IOException, bracketOnError, catches, displayException, try)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.INotify (Event (..), EventVariety (..), addWatch, initINotify)
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (fetchRepo, syncUserRepo, withUserRepoExclusiveIO)

{-# NOINLINE appliedConfig #-}
appliedConfig :: MVar (Maybe Text)
appliedConfig = unsafePerformIO (newMVar Nothing)

watchConfig :: IO ()
watchConfig = do
    path <- resolveConfigPath
    inotify <- initINotify
    let watched = Just (rawPath (takeFileName path))
    void $
        addWatch inotify [CloseWrite, MoveIn] (rawPath (takeDirectory path)) $ \event ->
            when (changedFile event == watched) (applyChangedFile path)
  where
    rawPath = encodeUtf8 . T.pack

changedFile :: Event -> Maybe ByteString
changedFile event = case event of
    Closed{maybeFilePath = name} -> name
    MovedIn{filePath = name} -> Just name
    _ -> Nothing

applyChangedFile :: FilePath -> IO ()
applyChangedFile path = modifyMVar_ appliedConfig $ \applied -> do
    loaded <- try (readConfigFile path)
    case loaded of
        Left (err :: IOException) -> applied <$ reportOutcome (Left (displayException err))
        Right contents
            | Just contents == applied -> pure applied
            | otherwise -> do
                putStrLn "Configuration changed, applying..."
                Just contents <$ (either (pure . Left) applyConfig (parseConfig contents) >>= reportOutcome)

readConfigText :: IO Text
readConfigText = resolveConfigPath >>= readConfigFile

modifyConfig :: (Text -> Either String Text) -> IO (Either String ())
modifyConfig edit = modifyMVar appliedConfig $ \applied -> do
    path <- resolveConfigPath
    current <- readConfigFile path
    case edit current >>= \contents -> (,) contents <$> parseConfig contents of
        Left err -> pure (applied, Left err)
        Right (contents, config) -> do
            writeAtomically path contents
            putStrLn "Configuration saved, applying..."
            outcome <- applyConfig config
            reportOutcome outcome
            pure (Just contents, outcome)

writeAtomically :: FilePath -> Text -> IO ()
writeAtomically path contents =
    bracketOnError
        (openTempFile (takeDirectory path) (takeFileName path))
        (\(staged, handle) -> hClose handle >> removeFile staged)
        ( \(staged, handle) -> do
            BS.hPut handle (encodeUtf8 contents)
            hClose handle
            renameFile staged path
        )

applyConfig :: Config -> IO (Either String ())
applyConfig config =
    ( do
        warnMissingProviderKeys config
        withUserRepoExclusiveIO (ExceptT (syncUserRepo config) >> fetchRepo)
    )
        `catches` [ Handler (\(err :: IOException) -> pure (Left (displayException err)))
                  , Handler (\(err :: ErrorCall) -> pure (Left (displayException err)))
                  ]

reportOutcome :: Either String () -> IO ()
reportOutcome = putStrLn . either ("Warning: Failed to apply configuration: " ++) (const "Configuration applied.")
