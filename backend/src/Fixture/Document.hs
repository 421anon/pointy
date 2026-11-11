{-# LANGUAGE OverloadedStrings #-}

module Fixture.Document (
    FixtureDocument (..),
    FixtureAgent (..),
    FixtureAgentEdit (..),
    loadDocument,
    jsonAnswer,
    rawAnswer,
    appliedAnswer,
    derivationAnswer,
    derivationOutputs,
    logAnswer,
    coldStorageDataset,
    pseudoHash,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object, toJSON, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, ord)
import Data.List (isInfixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import ProjectTree (stepEntries)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

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
    , documentAgent :: Maybe FixtureAgent
    , documentColdStorage :: Maybe (Map.Map String FixtureColdDataset)
    }

data FixtureColdDataset = FixtureColdDataset
    { coldDatasetPhase :: Text
    , coldDatasetLocal :: Bool
    , coldDatasetFiles :: [FixtureColdFile]
    }

data FixtureColdFile = FixtureColdFile
    { coldFilePath :: Text
    , coldFileLocal :: Bool
    , coldFileOnload :: Maybe FixtureColdOnload
    }

data FixtureColdOnload = FixtureColdOnload
    { coldOnloadDone :: Integer
    , coldOnloadTotal :: Integer
    }

data FixtureAgent = FixtureAgent
    { agentTitle :: Maybe Text
    , agentReply :: Maybe Text
    , agentEdits :: [FixtureAgentEdit]
    }

data FixtureAgentEdit = FixtureAgentEdit
    { editPath :: FilePath
    , editContent :: Maybe Text
    , editFind :: Maybe Text
    , editReplace :: Maybe Text
    }

instance FromJSON FixtureAgent where
    parseJSON = withObject "FixtureAgent" $ \obj ->
        FixtureAgent
            <$> obj .:? "title"
            <*> obj .:? "reply"
            <*> (obj .:? "edits" .!= Object KeyMap.empty >>= parseAgentEdits)

instance ToJSON FixtureAgent where
    toJSON agent =
        object
            [ "title" .= agentTitle agent
            , "reply" .= agentReply agent
            , "edits" .= agentEdits agent
            ]

instance FromJSON FixtureAgentEdit where
    parseJSON = withObject "FixtureAgentEdit" $ \obj ->
        FixtureAgentEdit
            <$> obj .: "path"
            <*> obj .:? "content"
            <*> obj .:? "find"
            <*> obj .:? "replace"

instance ToJSON FixtureAgentEdit where
    toJSON edit =
        object $
            ["path" .= editPath edit]
                ++ maybe [] (\content -> ["content" .= content]) (editContent edit)
                ++ maybe [] (\find -> ["find" .= find]) (editFind edit)
                ++ maybe [] (\replace -> ["replace" .= replace]) (editReplace edit)

parseAgentEdits :: Value -> Parser [FixtureAgentEdit]
parseAgentEdits (Object edits) = mapM (uncurry parseNamedEdit) (KeyMap.toList edits)
  where
    parseNamedEdit name = parseEditValue (Key.toText name)
parseAgentEdits (Array edits) = mapM parseJSON (Vector.toList edits)
parseAgentEdits value = fail ("agent.edits must be an object or array of edits, not " ++ show value)

parseEditValue :: Text -> Value -> Parser FixtureAgentEdit
parseEditValue path (String content) = pure (FixtureAgentEdit (T.unpack path) (Just content) Nothing Nothing)
parseEditValue path (Object fields) =
    FixtureAgentEdit (T.unpack path)
        <$> fields .:? "content"
        <*> fields .:? "find"
        <*> fields .:? "replace"
parseEditValue path value = fail ("agent edit for " ++ T.unpack path ++ " must be a string or object, not " ++ show value)

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
            <*> obj .:? "agent"
            <*> obj .:? "coldStorage"

instance FromJSON FixtureColdDataset where
    parseJSON = withObject "cold storage dataset" $ \o ->
        FixtureColdDataset
            <$> o .:? "phase" .!= "evicted"
            <*> o .:? "local" .!= False
            <*> (fromMaybe [] <$> o .:? "files")

instance ToJSON FixtureColdDataset where
    toJSON dataset =
        object
            [ "phase" .= coldDatasetPhase dataset
            , "local" .= coldDatasetLocal dataset
            , "files" .= coldDatasetFiles dataset
            ]

instance FromJSON FixtureColdFile where
    parseJSON = withObject "cold storage file" $ \o ->
        FixtureColdFile
            <$> o .: "path"
            <*> o .:? "local" .!= False
            <*> o .:? "onload"

instance ToJSON FixtureColdFile where
    toJSON file =
        object
            [ "path" .= coldFilePath file
            , "local" .= coldFileLocal file
            , "onload" .= coldFileOnload file
            ]

instance FromJSON FixtureColdOnload where
    parseJSON = withObject "cold storage onload" $ \o ->
        FixtureColdOnload
            <$> (fromMaybe 0 <$> o .:? "bytes_done")
            <*> (fromMaybe 0 <$> o .:? "bytes_total")

instance ToJSON FixtureColdOnload where
    toJSON onload =
        object
            [ "bytes_done" .= coldOnloadDone onload
            , "bytes_total" .= coldOnloadTotal onload
            ]

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
    | "pointy.root or null" `isInfixOf` applyExpr = Right (encodeValue (object ["missing" .= ("linked-data" :: Text)]))
    | "flake.outPath + \"/steps\"" `isInfixOf` applyExpr = Right (encodeValue (object []))
    | "attrNames" `isInfixOf` applyExpr, attr == "#pointy.projects" = Right (encodeValue (toJSON (projectIds document)))
    | "removeAttrs" `isInfixOf` applyExpr = jsonAnswer document attr
    | "project.certificates or project.outPaths" `isInfixOf` applyExpr = Right (encodeValue (projectStatusPaths document key))
    | "step.certificate or step" `isInfixOf` applyExpr, attr == "#pointy.steps" = maybe (Left "fixture has no build target for one of the steps") (Right . encodeValue . toJSON . map stepTarget) (traverse (statusPathOf document) (idsIn applyExpr))
    | "step.certificate or step" `isInfixOf` applyExpr = maybe (Left ("fixture has no build target for " ++ attr)) (Right . encodeValue . stepTarget) (statusPathOf document key)
    | "srcFiles" `isInfixOf` applyExpr = Right (either (const (encodeValue Null)) id (jsonAnswer document ("#pointy.steps." ++ fromMaybe "" (listToMaybe (idsIn applyExpr)) ++ ".srcFiles")))
    | "presets" `isInfixOf` applyExpr = Right (encodeValue (documentPresets document))
    | "notices" `isInfixOf` applyExpr = Right (encodeValue (entry (documentNotices document)))
    | "extras.drvPath" `isInfixOf` applyExpr = Right (encodeValue extrasTargetValue)
    | "extras.outPath" `isInfixOf` applyExpr = Right (encodeValue extrasValue)
    | stepEntries "p" `isInfixOf` applyExpr = Right (encodeValue (projectStepIdsAnswer (toJSON (map stepIdNumber (projectStepIds document (fromMaybe "" (listToMaybe (idsIn applyExpr))))))))
    | "reviewedRevision" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (\id_ -> [entryOf (documentReviews document) id_]) (idsIn applyExpr))))
    | "isProject" `isInfixOf` applyExpr = Right (encodeValue (rawProjectFiles document))
    | "toString id" `isInfixOf` applyExpr = Right (encodeValue (versioned (toJSON (Map.fromList [(stepId, fixtureKey stepId) | stepId <- idsIn applyExpr]))))
    | ".key); in if t.success" `isInfixOf` applyExpr = Right (encodeValue (keyAnswer applyExpr))
    | "certificate.success" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (stepPathsValue document) (idsIn applyExpr))))
    | "certificate" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (maybe Null String . statusPathOf document) (idsIn applyExpr))))
    | "tryEval" `isInfixOf` applyExpr = Right (encodeValue (toJSON (map (pathOf (documentOutPaths document)) (idsIn applyExpr))))
    | otherwise = Left ("fixture has no answer for " ++ applyExpr ++ " on " ++ attr)
  where
    key = lastSegment attr
    versioned value = object ["version" .= (1 :: Int), "value" .= value]
    projectStepIdsAnswer
        | "inherit version" `isInfixOf` applyExpr = versioned
        | otherwise = id
    stepIdNumber :: String -> Int
    stepIdNumber stepId = fromMaybe 0 (readMaybe stepId)
    entry mapping = fromMaybe Null (Map.lookup key mapping)
    entryOf mapping id_ = fromMaybe Null (Map.lookup id_ mapping)
    pathOf mapping id_ = maybe Null String (Map.lookup id_ mapping)
    extrasValue = case Map.lookup key (documentExtrasOutPaths document) of
        Just (Just path) -> String path
        _ -> Null
    extrasTargetValue = case Map.lookup key (documentExtrasOutPaths document) of
        Just (Just path) ->
            object
                [ "path" .= path
                , "drv" .= maybe (path <> ".drv") T.pack (derivationAnswer document (T.unpack path))
                ]
        _ -> Null
    stepTarget path = object ["certified" .= certified document, "path" .= path, "drv" .= maybe (path <> ".drv") T.pack (derivationAnswer document (T.unpack path))]

keyAnswer :: String -> Value
keyAnswer applyExpr = versioned (maybe Null fixtureKey (listToMaybe (idsIn applyExpr)))
  where
    versioned value = object ["version" .= (1 :: Int), "value" .= value]

fixtureKey :: String -> Value
fixtureKey stepId = String ("fixture-key-" <> T.pack stepId)

projectStepIds :: FixtureDocument -> String -> [String]
projectStepIds document pid =
    [ stepId | stepValue <- projectStepValues document pid, Just stepId <- [valueId stepValue] ]

rawProjectFiles :: FixtureDocument -> Value
rawProjectFiles document =
    toJSON
        [ object ["id" .= projectId, "payload" .= encodeValue project]
        | (attr, project) <- Map.toList (documentJson document)
        , Just projectId <- [readMaybe =<< stripPrefix "#pointy.projects." attr :: Maybe Int]
        ]

projectIds :: FixtureDocument -> [String]
projectIds document =
    [ name
    | attr <- Map.keys (documentJson document)
    , Just name <- [stripPrefix "#pointy.projects." attr]
    , all isDigit name
    ]

certified :: FixtureDocument -> Bool
certified = not . Map.null . documentCertificates

statusPathOf :: FixtureDocument -> String -> Maybe Text
statusPathOf document stepId =
    Map.lookup stepId (if certified document then documentCertificates document else documentOutPaths document)

stepPathsValue :: FixtureDocument -> String -> Value
stepPathsValue document stepId = case statusPathOf document stepId of
    Nothing -> Null
    Just certificate -> object ["certificate" .= certificate, "output" .= Map.findWithDefault certificate stepId (documentOutPaths document)]

projectStatusPaths :: FixtureDocument -> String -> Value
projectStatusPaths document pid =
    toJSON $
        Map.fromList
            [ (stepId, stepPathsValue document stepId)
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
    (segment, _ : _) -> reverse segment
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

derivationOutputs :: FixtureDocument -> FilePath -> [FilePath]
derivationOutputs document drv = [path | (path, deriver) <- Map.toList (documentDerivations document), deriver == drv]

coldStorageDataset :: FixtureDocument -> Text -> Maybe Value
coldStorageDataset document storePath =
    toJSON <$> (documentColdStorage document >>= Map.lookup (T.unpack storePath))

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
