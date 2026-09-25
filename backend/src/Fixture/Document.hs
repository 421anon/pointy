{-# LANGUAGE OverloadedStrings #-}

module Fixture.Document (
    FixtureDocument (..),
    loadDocument,
    jsonAnswer,
    rawAnswer,
    appliedAnswer,
    derivationAnswer,
    logAnswer,
    pseudoHash,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object, toJSON, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, ord)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import System.Directory (doesFileExist)

data FixtureDocument = FixtureDocument
    { documentBranch :: Text
    , documentJson :: Map.Map String Value
    , documentRaw :: Map.Map String Text
    , documentPresets :: Value
    , documentOutPaths :: Map.Map String Text
    , documentCertificates :: Map.Map String Text
    , documentExtrasOutPaths :: Map.Map String (Maybe Text)
    , documentNotices :: Map.Map String Value
    , documentReviews :: Map.Map String Value
    , documentProjectStepIds :: Map.Map String Value
    , documentValidPaths :: [FilePath]
    , documentDerivations :: Map.Map FilePath FilePath
    , documentLogs :: Map.Map FilePath FilePath
    , documentReferences :: Map.Map FilePath [FilePath]
    , documentOutputs :: Map.Map FilePath [FilePath]
    }

instance FromJSON FixtureDocument where
    parseJSON = withObject "FixtureDocument" $ \obj ->
        FixtureDocument
            <$> obj .:? "branch" .!= "screenshots"
            <*> (Map.mapKeys T.unpack <$> obj .:? "json" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "raw" .!= Map.empty)
            <*> obj .:? "presets" .!= Null
            <*> (Map.mapKeys T.unpack <$> obj .:? "outPaths" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "certificates" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "extrasOutPaths" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "notices" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "reviews" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "projectStepIds" .!= Map.empty)
            <*> obj .:? "valid" .!= []
            <*> (Map.mapKeys T.unpack <$> obj .:? "derivations" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "logs" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "references" .!= Map.empty)
            <*> (Map.mapKeys T.unpack <$> obj .:? "outputs" .!= Map.empty)

loadDocument :: FilePath -> IO (Either String FixtureDocument)
loadDocument path = eitherDecode <$> LBS.readFile path

jsonAnswer :: FixtureDocument -> String -> Either String String
jsonAnswer document attr =
    maybe (Left ("fixture has no answer for " ++ attr)) (Right . encodeValue) (Map.lookup attr (documentJson document))

rawAnswer :: FixtureDocument -> String -> Either String String
rawAnswer document attr =
    maybe (Left ("fixture has no raw answer for " ++ attr)) (Right . T.unpack) (Map.lookup attr (documentRaw document))

appliedAnswer :: FixtureDocument -> String -> String -> Either String String
appliedAnswer document applyExpr attr
    | "removeAttrs" `isInfixOf` applyExpr = jsonAnswer document attr
    | "project.certificates or project.outPaths" `isInfixOf` applyExpr = Right (encodeValue (projectStatusPaths document key))
    | "certified" `isInfixOf` applyExpr = maybe (Left ("fixture has no build target for " ++ attr)) (Right . encodeValue . stepTarget) (statusPathOf document key)
    | "srcFiles" `isInfixOf` applyExpr = Right (either (const (encodeValue Null)) id (jsonAnswer document (attr ++ ".srcFiles")))
    | "presets" `isInfixOf` applyExpr = Right (encodeValue (documentPresets document))
    | "notices" `isInfixOf` applyExpr = Right (encodeValue (entry (documentNotices document)))
    | "extras.outPath" `isInfixOf` applyExpr = Right (encodeValue extrasValue)
    | "step.def.id" `isInfixOf` applyExpr = Right (encodeValue (maybe Null (entryOf (documentProjectStepIds document)) (listToMaybe (idsIn applyExpr))))
    | "reviewedRevision" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (\id_ -> [entryOf (documentReviews document) id_]) (idsIn applyExpr))))
    | "certificate" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (maybe Null String . statusPathOf document) (idsIn applyExpr))))
    | "tryEval" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (pathOf (documentOutPaths document)) (idsIn applyExpr))))
    | otherwise = Left ("fixture has no answer for " ++ applyExpr ++ " on " ++ attr)
  where
    key = lastSegment attr
    entry mapping = fromMaybe Null (Map.lookup key mapping)
    entryOf mapping id_ = fromMaybe Null (Map.lookup id_ mapping)
    pathOf mapping id_ = maybe Null String (Map.lookup id_ mapping)
    extrasValue = case Map.lookup key (documentExtrasOutPaths document) of
        Just (Just path) -> String path
        _ -> Null
    stepTarget path = object ["certified" .= certified document, "path" .= path, "drv" .= (path <> ".drv")]

certified :: FixtureDocument -> Bool
certified = not . Map.null . documentCertificates

statusPathOf :: FixtureDocument -> String -> Maybe Text
statusPathOf document stepId =
    Map.lookup stepId (if certified document then documentCertificates document else documentOutPaths document)

projectStatusPaths :: FixtureDocument -> String -> Value
projectStatusPaths document pid =
    toJSON $
        Map.fromList
            [ (stepId, fromMaybe "/invalid" (statusPathOf document stepId))
            | stepValue <- projectStepValues document pid
            , Just stepId <- [valueId stepValue]
            ]

projectStepValues :: FixtureDocument -> String -> [Value]
projectStepValues document pid =
    case Map.lookup pid (documentProjectStepIds document) of
        Just (Array values) -> Vector.toList values
        Just value -> [value]
        Nothing -> []

valueId :: Value -> Maybe String
valueId (String text) = Just (T.unpack text)
valueId (Number number) = Just (show (floor number :: Integer))
valueId _ = Nothing

lastSegment :: String -> String
lastSegment attr = case break (== '.') (reverse attr) of
    (segment, _ : rest) -> reverse segment
    (segment, []) -> reverse segment

idsIn :: String -> [String]
idsIn expression = filter isIdentifier (quotedTokens expression)
  where
    isIdentifier token = not (null token) && all isDigit token

quotedTokens :: String -> [String]
quotedTokens value = case break (== '"') value of
    (_, []) -> []
    (_, _ : rest) -> case break (== '"') rest of
        (token, _ : after) -> token : quotedTokens after
        (_, []) -> []

derivationAnswer :: FixtureDocument -> FilePath -> Maybe FilePath
derivationAnswer document path = Map.lookup path (documentDerivations document)

logAnswer :: FixtureDocument -> FilePath -> IO (Maybe String)
logAnswer document drv = case Map.lookup drv (documentLogs document) of
    Nothing -> pure Nothing
    Just path -> do
        exists <- doesFileExist path
        if exists then Just <$> readFile path else pure Nothing
encodeValue :: Value -> String
encodeValue = T.unpack . TE.decodeUtf8 . LBS.toStrict . A.encode

pseudoHash :: FilePath -> Text
pseudoHash path = "sha256-" <> T.pack (take 52 (map digit (drop 1 (iterate next (seed path)))))
  where
    seed = foldl (\acc char -> (acc * 31 + ord char) `mod` 2147483647) 7
    next value = (value * 1103515245 + 12345) `mod` 2147483648
    digit value = alphabet !! fromIntegral (value `mod` 32)
    alphabet = "0123456789abcdfghijklmnpqrsvwxyz"
