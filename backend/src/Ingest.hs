{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ingest (
    IngestResult (..),
    ingestProgram,
    runIngest,
    storeRefName,
) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Control.Monad (void)
import qualified Data.Aeson as A
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data IngestResult = IngestResult
    { ingestStorePath :: FilePath
    , ingestNarHash :: Text
    , ingestNarSize :: Integer
    , ingestReferencesSource :: Bool
    }
    deriving (Eq, Show)

ingestProgram :: FilePath
ingestProgram = "pointy-ingest"

storeRefName :: String
storeRefName = "store-ref"

data IngestLine
    = IngestProgress Integer Integer
    | IngestFinished IngestResult
    | IngestFailed String

runIngest :: FilePath -> String -> (Integer -> Integer -> IO ()) -> IO (Either String IngestResult)
runIngest directory name reportProgress = do
    spawned <- try (createProcess (proc ingestProgram [directory, name]){std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe})
    case spawned of
        Left (err :: IOException) -> pure $ Left ("failed to start " ++ ingestProgram ++ ": " ++ show err)
        Right (_, mOut, mErr, handle) -> do
            errReader <- case mErr of
                Just errHandle -> Just <$> async (readHandle errHandle)
                Nothing -> pure Nothing
            final <- case mOut of
                Just outHandle -> consume outHandle reportProgress
                Nothing -> pure Nothing
            closeMaybe mOut
            exitCode <- waitForProcess handle
            errors <- maybe (pure "") wait errReader
            closeMaybe mErr
            pure $ case final of
                Just (IngestFinished result) | exitCode == ExitSuccess -> Right result
                Just (IngestFailed message) -> Left message
                _ -> Left (failureMessage exitCode errors)

consume :: Handle -> (Integer -> Integer -> IO ()) -> IO (Maybe IngestLine)
consume handle reportProgress = go Nothing
  where
    go final = do
        line <- try (BS.hGetLine handle)
        case line of
            Left (_ :: IOException) -> pure final
            Right bytes -> case parseIngestLine bytes of
                Just (IngestProgress done total) -> reportProgress done total >> go final
                Just finished -> go (Just finished)
                Nothing -> go final

parseIngestLine :: BS.ByteString -> Maybe IngestLine
parseIngestLine bytes = A.decodeStrict bytes >>= fromValue

fromValue :: A.Value -> Maybe IngestLine
fromValue (A.Object object_) = case KM.lookup "progress" object_ of
    Just (A.Object progress) -> IngestProgress <$> integerField progress "done" <*> integerField progress "total"
    _ -> case KM.lookup "ok" object_ of
        Just (A.Bool True) -> IngestFinished <$> result
        Just (A.Bool False) -> Just (IngestFailed (maybe "ingest program reported failure" T.unpack (textField object_ "error")))
        _ -> Nothing
  where
    result =
        IngestResult
            <$> (T.unpack <$> textField object_ "store_path")
            <*> textField object_ "nar_hash"
            <*> integerField object_ "nar_size"
            <*> boolField object_ "references_source"

textField :: KM.KeyMap A.Value -> Key -> Maybe Text
textField object_ key = case KM.lookup key object_ of
    Just (A.String value) -> Just value
    _ -> Nothing

integerField :: KM.KeyMap A.Value -> Key -> Maybe Integer
integerField object_ key = case KM.lookup key object_ of
    Just (A.Number value) -> case floatingOrInteger value :: Either Double Integer of
        Right integer -> Just integer
        Left _ -> Nothing
    _ -> Nothing

boolField :: KM.KeyMap A.Value -> Key -> Maybe Bool
boolField object_ key = case KM.lookup key object_ of
    Just (A.Bool value) -> Just value
    _ -> Nothing

failureMessage :: ExitCode -> String -> String
failureMessage exitCode errors
    | not (null errors) = errors
    | exitCode == ExitSuccess = ingestProgram ++ " produced no result"
    | otherwise = ingestProgram ++ " failed with " ++ show exitCode

readHandle :: Handle -> IO String
readHandle handle = do
    bytes <- BS.hGetContents handle
    pure (T.unpack (T.strip (TE.decodeUtf8With lenientDecode bytes)))

closeMaybe :: Maybe Handle -> IO ()
closeMaybe Nothing = pure ()
closeMaybe (Just handle) = void (try (hClose handle) :: IO (Either IOException ()))
