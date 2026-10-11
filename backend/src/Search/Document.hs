{-# LANGUAGE OverloadedStrings #-}

module Search.Document (
    StepDocument (..),
    buildDocuments,
) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Object, Value (..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Effectful (Eff, IOE, liftIO, (:>))
import Effects (Eval)
import ProjectFiles (RawProjectFile (..), loadRawProjectFilesAt, loadUnfiledStepEntries)
import ProjectTree (ChildRef (..), normalizeProjects, projectChildRefs)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext, runNixEvalJsonInRepo)

data StepDocument = StepDocument
    { documentStepId :: Int
    , documentName :: Text
    , documentText :: Text
    , documentHash :: Text
    }
    deriving (Show)

data StepEntry = StepEntry
    { entryId :: Int
    , entryName :: Text
    , entryType :: Text
    , entryNote :: Text
    , entryArgValues :: [Value]
    }

buildDocuments :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) [StepDocument]
buildDocuments ctx = do
    rawFiles <- loadRawProjectFilesAt ctx
    let projects =
            normalizeProjects $
                Object $
                    KeyMap.fromList
                        [ (Key.fromText (T.pack (show (rawFileId rawFile))), payload)
                        | rawFile <- rawFiles
                        , Just payload <- [decodePayload (rawFilePayload rawFile)]
                        ]
    stepConfigOutput <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
    stepConfig <- either (throwError . ("Failed to read the step configuration: " ++)) pure (decodeJson stepConfigOutput)
    let entries = stepEntriesOf projects
    unfiledResult <- lift $ runExceptT (loadUnfiledStepEntries ctx (map entryId entries))
    unfiled <- case unfiledResult of
        Left err -> do
            liftIO (putStrLn ("Warning: Failed to read the unfiled steps: " ++ err))
            pure []
        Right steps -> pure steps
    let allEntries = entries ++ mapMaybe unfiledEntryOf unfiled
        names = Map.fromList [(entryId entry, entryName entry) | entry <- allEntries]
        paths = folderPaths projects
    pure
        [ StepDocument
            { documentStepId = entryId entry
            , documentName = entryName entry
            , documentText = text
            , documentHash = hexDigest (hash (TE.encodeUtf8 text) :: Digest SHA256)
            }
        | entry <- allEntries
        , let text = documentTextFor stepConfig names paths entry
        ]

unfiledEntryOf :: Value -> Maybe StepEntry
unfiledEntryOf (Object child) = entryOf child
unfiledEntryOf _ = Nothing

decodePayload :: Maybe Text -> Maybe Value
decodePayload Nothing = Nothing
decodePayload (Just payload) = either (const Nothing) Just (decodeJson (T.unpack payload))

decodeJson :: String -> Either String Value
decodeJson = eitherDecode . BL.fromStrict . TE.encodeUtf8 . T.pack

hexDigest :: Digest a -> Text
hexDigest = TE.decodeUtf8 . Base16.encode . convert

projectIds :: Value -> [Int]
projectIds (Object projects) =
    mapMaybe projectIdOf (KeyMap.toList projects)
  where
    projectIdOf (key, _) = readMaybe (T.unpack (Key.toText key))
projectIds _ = []

folderPaths :: Value -> Map Int Text
folderPaths projects = Map.fromListWith (\_ first -> first) (concatMap (walk Set.empty []) roots)
  where
    ids = projectIds projects
    nestedIds = Set.fromList [childId | projectId <- ids, ProjectChild childId <- projectChildRefsOf projects projectId]
    roots = filter (`Set.notMember` nestedIds) ids
    walk visited ancestors projectId =
        let path = if projectId == 0 then [] else ancestors ++ [projectName projects projectId]
            label = T.intercalate " / " path
            children = projectChildRefsOf projects projectId
            visited' = Set.insert projectId visited
            here = [(stepId, label) | StepChild stepId <- children]
            nested = [walk visited' path childId | ProjectChild childId <- children, Set.notMember childId visited']
         in here ++ concat nested

projectObject :: Value -> Int -> Maybe Object
projectObject (Object projects) projectId = case KeyMap.lookup (Key.fromText (T.pack (show projectId))) projects of
    Just (Object project) -> Just project
    _ -> Nothing
projectObject _ _ = Nothing

projectName :: Value -> Int -> Text
projectName projects projectId = case projectObject projects projectId of
    Just project -> case KeyMap.lookup "name" project of
        Just (String name) -> name
        _ -> T.pack (show projectId)
    Nothing -> T.pack (show projectId)

projectChildRefsOf :: Value -> Int -> [ChildRef]
projectChildRefsOf projects projectId = maybe [] projectChildRefs (projectObject projects projectId)

stepEntriesOf :: Value -> [StepEntry]
stepEntriesOf projects =
    [ entry
    | projectId <- projectIds projects
    , Just project <- [projectObject projects projectId]
    , Just (Array children) <- [KeyMap.lookup "children" project]
    , Object child <- V.toList children
    , Just entry <- [entryOf child]
    ]

entryOf :: Object -> Maybe StepEntry
entryOf child = do
    Object inner <- KeyMap.lookup "step" child
    stepId <- case KeyMap.lookup "id" inner of
        Just (Number value) -> toBoundedInteger value
        _ -> Nothing
    let definition = case KeyMap.lookup "def" inner of
            Just (Object def) -> def
            _ -> KeyMap.empty
        textField key = case KeyMap.lookup key definition of
            Just (String value) -> value
            _ -> ""
    Just
        StepEntry
            { entryId = stepId
            , entryName = if T.null (textField "name") then T.pack (show stepId) else textField "name"
            , entryType = textField "type"
            , entryNote = textField "note"
            , entryArgValues = case KeyMap.lookup "args" definition of
                Just (Object fields) -> map snd (KeyMap.toList fields)
                _ -> []
            }

flattenArg :: Map Int Text -> Value -> [Text]
flattenArg _ (String value) = [value]
flattenArg _ (Bool True) = ["true"]
flattenArg _ (Bool False) = ["false"]
flattenArg _ (Number value) = [T.pack (show value)]
flattenArg names (Array items) = concatMap (flattenArg names) (V.toList items)
flattenArg names (Object fields) =
    case (KeyMap.lookup "step" fields, KeyMap.lookup "hash" fields) of
        (Just (Number stepId), _) ->
            case toBoundedInteger stepId :: Maybe Int of
                Just stepReference -> [fromMaybe (T.pack (show stepReference)) (Map.lookup stepReference names)]
                Nothing -> []
        (_, Just (String contentHash)) -> [contentHash]
        _ -> concatMap (flattenArg names . snd) (KeyMap.toList fields)
flattenArg _ Null = []

documentTextFor :: Value -> Map Int Text -> Map Int Text -> StepEntry -> Text
documentTextFor stepConfig names paths entry =
    T.intercalate
        "\n"
        ( filter
            (not . T.null)
            [ "Step: " <> entryName entry
            , "Type: " <> typeLabel
            ]
            ++ folderLine
            ++ ["Note: " <> entryNote entry | not (T.null (entryNote entry))]
            ++ ["Arguments: " <> args | not (T.null args)]
        )
  where
    args = T.intercalate ", " (concatMap (flattenArg names) (entryArgValues entry))
    folderLine = case Map.lookup (entryId entry) paths of
        Just path | not (T.null path) -> ["Folder: " <> path]
        Just _ -> []
        Nothing -> ["Folder: unfiled"]
    typeLabel = case Map.lookup (entryType entry) typeNames of
        Just label -> label
        Nothing -> entryType entry
    typeNames =
        Map.fromList
            [ (Key.toText name, label)
            | (name, Object config) <- KeyMap.toList stepConfigObject
            , Just (String label) <- [KeyMap.lookup "name" config]
            ]
    stepConfigObject = case stepConfig of
        Object fields -> fields
        _ -> KeyMap.empty
