{-# LANGUAGE OverloadedStrings #-}

module Search.Document (
    StepDocument (..),
    buildDocuments,
) where

import Certificates (evalProjectDefinitions)
import Control.Monad.Except (ExceptT, throwError)
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
import Data.Scientific (FPFormat (Fixed), floatingOrInteger, formatScientific, toBoundedInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Effectful (Eff, IOE, (:>))
import Effects (Eval)
import ProjectTree (ChildRef (..), projectChildRefs)
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
    , entryArgs :: [(Text, Value)]
    }

buildDocuments :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) [StepDocument]
buildDocuments ctx = do
    projects <- evalProjectDefinitions ctx
    stepConfigOutput <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
    stepConfig <- either (throwError . ("Failed to read the step configuration: " ++)) pure (decodeJson stepConfigOutput)
    let entries = stepEntriesOf projects
        names = Map.fromList [(entryId entry, entryName entry) | entry <- entries]
        paths = folderPaths projects
    pure
        [ StepDocument
            { documentStepId = entryId entry
            , documentName = entryName entry
            , documentText = text
            , documentHash = hexDigest (hash (TE.encodeUtf8 text) :: Digest SHA256)
            }
        | entry <- entries
        , let text = documentTextFor stepConfig names paths entry
        ]

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
            , entryArgs = case KeyMap.lookup "args" definition of
                Just (Object fields) -> [(Key.toText key, value) | (key, value) <- KeyMap.toList fields]
                _ -> []
            }

flattenArg :: Map Int Text -> Value -> [Text]
flattenArg _ (String value) = [value]
flattenArg _ (Bool True) = ["true"]
flattenArg _ (Bool False) = ["false"]
flattenArg _ (Number value) = [either (const (T.pack (formatScientific Fixed Nothing value))) (T.pack . show) (floatingOrInteger value :: Either Double Integer)]
flattenArg names (Array items) = concatMap (flattenArg names) (V.toList items)
flattenArg names (Object fields) =
    case (KeyMap.lookup "step" fields, KeyMap.lookup "hash" fields) of
        (Just (Number stepId), _) ->
            case toBoundedInteger stepId :: Maybe Int of
                Just stepReference -> [fromMaybe (T.pack (show stepReference)) (Map.lookup stepReference names)]
                Nothing -> []
        (_, Just (String _)) -> []
        _ -> concatMap (flattenArg names . snd) (KeyMap.toList fields)
flattenArg _ Null = []

maxArgumentLength :: Int
maxArgumentLength = 1500

maxDocumentLength :: Int
maxDocumentLength = 6000

documentTextFor :: Value -> Map Int Text -> Map Int Text -> StepEntry -> Text
documentTextFor stepConfig names paths entry =
    T.take maxDocumentLength $
        T.intercalate
            "\n"
            ( filter
                (not . T.null)
                [ "Step: " <> entryName entry
                , "Type: " <> typeLabel
                ]
                ++ folderLine
                ++ ["Note: " <> entryNote entry | not (T.null (entryNote entry))]
                ++ argumentLines
            )
  where
    argumentLines =
        [ fieldLabel key <> ": " <> T.take maxArgumentLength (T.intercalate ", " values)
        | (key, value) <- entryArgs entry
        , let values = filter (not . T.null . T.strip) (flattenArg names value)
        , not (null values)
        ]
    folderLine = case Map.lookup (entryId entry) paths of
        Just path | not (T.null path) -> ["Folder: " <> path]
        _ -> []
    typeConfig = case stepConfig of
        Object root
            | Just (Object templates) <- KeyMap.lookup "templates" root
            , Just (Object config) <- KeyMap.lookup (Key.fromText (entryType entry)) templates ->
                config
        _ -> KeyMap.empty
    typeLabel = case KeyMap.lookup "displayName" typeConfig of
        Just (String label) | not (T.null label) -> label
        _ -> entryType entry
    fieldLabels =
        Map.fromList
            [ (name, label)
            | Just (Array fields) <- [KeyMap.lookup "fields" typeConfig]
            , Object field <- V.toList fields
            , Just (String name) <- [KeyMap.lookup "name" field]
            , let label = case KeyMap.lookup "label" field of
                    Just (String text) | not (T.null text) -> text
                    _ -> name
            ]
    fieldLabel key = Map.findWithDefault key key fieldLabels
