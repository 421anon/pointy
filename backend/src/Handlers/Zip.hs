module Handlers.Zip (ZipItem(..), listZipDirectory, safeZipPath, readZipFile) where

import Data.List (break, isPrefixOf)
import Control.Exception (SomeException, evaluate, try)
import System.FilePath.Posix (pathSeparator)
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

data ZipItem = ZipItem { zipItemName :: FilePath, zipItemIsDirectory :: Bool, zipItemSize :: Integer }
    deriving (Eq, Show)

safeZipPath :: FilePath -> Maybe ([FilePath], Bool)
safeZipPath "" = Nothing
safeZipPath path
    | isPrefixOf [pathSeparator] path = Nothing
    | '\NUL' `elem` path = Nothing
    | otherwise = case validateComponents components of
        Just comps -> Just (comps, trailingSlash)
        Nothing    -> Nothing
  where
    trailingSlash = not (null path) && last path == pathSeparator
    parts = splitOn pathSeparator path
    components = if trailingSlash then init parts else parts

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
        (pre, "")    -> [pre]
        (pre, _:suf) -> pre : splitOn c suf

    validateComponents :: [String] -> Maybe [String]
    validateComponents [] = Just []
    validateComponents (x:xs)
        | null x    = Nothing
        | x == "."  = Nothing
        | x == ".." = Nothing
        | otherwise = (x :) <$> validateComponents xs

listZipDirectory :: FilePath -> FilePath -> IO (Either String [ZipItem])
listZipDirectory zipPath dirPath = do
    lbs <- LBS.readFile zipPath
    case Zip.toArchiveOrFail lbs of
        Left err -> pure $ Left err
        Right archive -> pure $ processArchive archive
  where
    processArchive archive = case validateDirPath dirPath of
        Left err -> Left err
        Right targetComps ->
            let entries = Zip.zEntries archive
                childMap = foldr (processEntry targetComps) Map.empty entries
            in Right $ Map.elems childMap

    validateDirPath "" = Right []
    validateDirPath path = case safeZipPath path of
        Just (comps, trailingSlash)
            | not trailingSlash -> Right comps
        _ -> Left "Invalid ZIP directory path"

    processEntry targetComps entry = case safeZipPath (Zip.eRelativePath entry) of
        Nothing -> id
        Just (comps, entryTrailingSlash)
            | not (targetComps `isPrefixOf` comps) -> id
            | otherwise ->
                let rest = drop (length targetComps) comps
                in case rest of
                    [] -> id
                    (childName : _) ->
                        let remainingCount = length rest
                            isDir = remainingCount > 1 || entryTrailingSlash
                            item = ZipItem
                                { zipItemName = childName
                                , zipItemIsDirectory = isDir
                                , zipItemSize = if isDir then 0 else fromIntegral (Zip.eUncompressedSize entry)
                                }
                        in Map.insertWith mergeDuplicate childName item

    mergeDuplicate new existing
        | zipItemIsDirectory existing = existing
        | zipItemIsDirectory new = new
        | otherwise = existing

readZipFile :: FilePath -> FilePath -> IO (Either String (Integer, LBS.ByteString))
readZipFile zipPath filePath = case safeZipPath filePath of
    Nothing -> pure $ Left "Invalid ZIP file path"
    Just (targetComps, trailingSlash)
        | trailingSlash -> pure $ Left "Invalid ZIP file path"
        | otherwise -> do
            lbs <- LBS.readFile zipPath
            case Zip.toArchiveOrFail lbs of
                Left err -> pure $ Left err
                Right archive -> findInEntries targetComps (Zip.zEntries archive)
  where
    findInEntries _ [] = pure $ Left "ZIP entry not found"
    findInEntries targetComps (entry : rest) =
        case safeZipPath (Zip.eRelativePath entry) of
            Just (entryComps, False) | entryComps == targetComps ->
                if Zip.isEncryptedEntry entry
                    then pure $ Left "Encrypted ZIP entries are not supported"
                    else do
                        let content = Zip.fromEntry entry
                        result <- try $ evaluate $ LBS.length content
                        case result of
                            Left e -> pure $ Left $ show (e :: SomeException)
                            Right _ -> pure $ Right (fromIntegral (Zip.eUncompressedSize entry), content)
            _ -> findInEntries targetComps rest
