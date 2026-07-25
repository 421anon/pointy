{-# LANGUAGE TypeApplications #-}

module Handlers.Zip (ZipItem(..), listZipDirectory, safeZipPath, readZipFile) where

import Data.List (break, find, isPrefixOf)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (guard, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import System.FilePath.Posix (pathSeparator)
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

data ZipItem = ZipItem { zipItemName :: FilePath, zipItemIsDirectory :: Bool, zipItemSize :: Integer }
    deriving (Eq, Show)

safeZipPath :: FilePath -> Maybe ([FilePath], Bool)
safeZipPath path = do
    guard $ not (null path) && head path /= pathSeparator && '\NUL' `notElem` path
    let trailingSlash = last path == pathSeparator
        parts = splitOn pathSeparator path
        components = if trailingSlash then init parts else parts
    guard $ all (\component -> not (null component) && component /= "." && component /= "..") components
    pure (components, trailingSlash)
  where
    splitOn c s = case break (== c) s of
        (prefix, "") -> [prefix]
        (prefix, _ : suffix) -> prefix : splitOn c suffix

readArchive :: FilePath -> ExceptT String IO Zip.Archive
readArchive path = liftIO (LBS.readFile path) >>= liftEither . Zip.toArchiveOrFail

listZipDirectory :: FilePath -> FilePath -> IO (Either String [ZipItem])
listZipDirectory zipPath dirPath = runExceptT $ do
    archive <- readArchive zipPath
    targetComponents <- liftEither $ validateDirPath dirPath
    pure . Map.elems $ foldr (processEntry targetComponents) Map.empty (Zip.zEntries archive)
  where
    validateDirPath "" = Right []
    validateDirPath path = case safeZipPath path of
        Just (components, False) -> Right components
        _ -> Left "Invalid ZIP directory path"

    processEntry targetComponents entry items = case safeZipPath (Zip.eRelativePath entry) of
        Just (components, trailingSlash)
            | targetComponents `isPrefixOf` components
            , childName : descendants <- drop (length targetComponents) components
            , let isDirectory = trailingSlash || not (null descendants)
                  size = if isDirectory then 0 else fromIntegral (Zip.eUncompressedSize entry)
            -> Map.insertWith mergeDuplicate childName (ZipItem childName isDirectory size) items
        _ -> items

    mergeDuplicate new existing
        | zipItemIsDirectory existing = existing
        | zipItemIsDirectory new = new
        | otherwise = existing

readZipFile :: FilePath -> FilePath -> IO (Either String (Integer, LBS.ByteString))
readZipFile zipPath filePath = runExceptT $ do
    (targetComponents, trailingSlash) <-
        maybe (throwError "Invalid ZIP file path") pure $ safeZipPath filePath
    when trailingSlash $ throwError "Invalid ZIP file path"
    archive <- readArchive zipPath
    let matches entry = safeZipPath (Zip.eRelativePath entry) == Just (targetComponents, False)
    entry <- maybe (throwError "ZIP entry not found") pure $ find matches (Zip.zEntries archive)
    when (Zip.isEncryptedEntry entry) $ throwError "Encrypted ZIP entries are not supported"
    let content = Zip.fromEntry entry
    _ <- ExceptT $ first show <$> try @SomeException (evaluate $ LBS.length content)
    pure (fromIntegral $ Zip.eUncompressedSize entry, content)
