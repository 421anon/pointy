{-# LANGUAGE OverloadedStrings #-}

module ProjectTree (
    ChildRef (..),
    ChildUpdate (..),
    ChildChanges (..),
    ProjectFields (..),
    describeChild,
    newProject,
    normalizeProjects,
    normalizeProject,
    stepEntries,
    projectStepEntries,
    appendChildren,
    appendMissingChildren,
    applyChildChanges,
) where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, withObject, (.!=), (.:), (.:!), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

data ChildRef
    = StepChild Int
    | ProjectChild Int
    deriving (Eq, Show)

instance FromJSON ChildRef where
    parseJSON = withObject "ChildRef" (fmap fst . childEntry)

data ChildUpdate = ChildUpdate
    { updatedChild :: ChildRef
    , updatedHidden :: Maybe Bool
    , updatedSortKey :: Maybe (Maybe Int)
    }
    deriving (Eq, Show)

instance FromJSON ChildUpdate where
    parseJSON = withObject "ChildUpdate" $ \fields -> do
        (child, entry) <- childEntry fields
        ChildUpdate child <$> entry .:? "hidden" <*> entry .:! "sortKey"

data ChildChanges = ChildChanges
    { changedChildren :: [ChildUpdate]
    , removedChildren :: [ChildRef]
    }
    deriving (Eq, Show)

instance FromJSON ChildChanges where
    parseJSON = withObject "ChildChanges" $ \fields ->
        ChildChanges <$> fields .:? "update" .!= [] <*> fields .:? "remove" .!= []

data ProjectFields = ProjectFields
    { projectName :: Text
    , projectPreset :: Maybe Text
    , projectTemplates :: Maybe [Text]
    }
    deriving (Eq, Show)

instance FromJSON ProjectFields where
    parseJSON = withObject "ProjectFields" $ \fields ->
        ProjectFields <$> fields .: "name" <*> fields .:? "preset" <*> fields .:? "templates"

instance ToJSON ProjectFields where
    toJSON = object . projectFieldPairs

newProject :: ProjectFields -> Value
newProject fields = object (("children" .= ([] :: [Value])) : projectFieldPairs fields)

projectFieldPairs :: ProjectFields -> [Pair]
projectFieldPairs (ProjectFields name preset templates) =
    ("name" .= name) : catMaybes [("preset" .=) <$> preset, ("templates" .=) <$> templates]

childEntry :: Object -> Parser (ChildRef, Object)
childEntry fields = case (KeyMap.lookup "step" fields, KeyMap.lookup "project" fields) of
    (Just entry, Nothing) -> withObject "step" (identified StepChild) entry
    (Nothing, Just entry) -> withObject "project" (identified ProjectChild) entry
    _ -> fail "expected exactly one of \"step\" and \"project\""
  where
    identified tag entry = (\childId_ -> (tag childId_, entry)) <$> entry .: "id"

childKind :: ChildRef -> Text
childKind (StepChild _) = "step"
childKind (ProjectChild _) = "project"

childId :: ChildRef -> Int
childId (StepChild stepId) = stepId
childId (ProjectChild projectId) = projectId

describeChild :: ChildRef -> String
describeChild child = T.unpack (childKind child) ++ " " ++ show (childId child)

normalizeProjects :: Value -> Value
normalizeProjects (Object projects)
    | any (isJust . legacySteps) projects = Object (withRoot (KeyMap.map normalizeProject projects))
  where
    withRoot normalized
        | KeyMap.member "0" projects = normalized
        | otherwise = KeyMap.insert "0" (legacyRoot projects) normalized
normalizeProjects projects = projects

normalizeProject :: Value -> Value
normalizeProject project = case legacySteps project of
    Just (fields, steps) -> Object (KeyMap.insert "children" (Array (V.map stepChild steps)) (foldr KeyMap.delete fields ["steps", "hidden", "sortKey"]))
    Nothing -> project

legacySteps :: Value -> Maybe (Object, V.Vector Value)
legacySteps (Object fields)
    | not (KeyMap.member "children" fields)
    , Just (Array steps) <- KeyMap.lookup "steps" fields =
        Just (fields, steps)
legacySteps _ = Nothing

stepChild :: Value -> Value
stepChild step = object ["step" .= withStepId step]
  where
    withStepId (Object entry)
        | Just (Object def) <- KeyMap.lookup "def" entry
        , Just stepId <- KeyMap.lookup "id" def =
            Object (KeyMap.insert "id" stepId entry)
    withStepId entry = entry

legacyRoot :: Object -> Value
legacyRoot projects =
    object
        [ "id" .= (0 :: Int)
        , "name" .= ("Home" :: Text)
        , "preset" .= Null
        , "templates" .= ([] :: [Text])
        , "validationErrors" .= ([] :: [Text])
        , "children" .= map rootEntry (sortOn fst [(projectId, project) | (key, project) <- KeyMap.toList projects, Just projectId <- [readMaybe (Key.toString key)]])
        ]
  where
    rootEntry (projectId, project) =
        object ["project" .= object ["id" .= (projectId :: Int), "hidden" .= former "hidden" (Bool False) project, "sortKey" .= former "sortKey" Null project]]
    former key fallback (Object fields) = fromMaybe fallback (KeyMap.lookup key fields)
    former _ fallback _ = fallback

stepEntries :: String -> String
stepEntries project =
    "(if " ++ project ++ " ? children then map (c: c.step) (builtins.filter (c: c ? step) " ++ project ++ ".children) else " ++ project ++ ".steps or [ ])"

projectStepEntries :: String -> Int -> String
projectStepEntries projects projectId =
    "(let p = " ++ projects ++ "." ++ show (show projectId) ++ " or " ++ absent ++ "; in " ++ stepEntries "p" ++ ")"
  where
    absent
        | projectId == 0 = "{ }"
        | otherwise = "(throw \"Project " ++ show projectId ++ " does not exist.\")"

appendChildren :: [ChildRef] -> Text
appendChildren children =
    "orig // { children = orig.children ++ " <> nixList (map newEntry children) <> "; }"

appendMissingChildren :: [ChildRef] -> Text
appendMissingChildren children =
    entryMatching
        <> "orig // { children = orig.children ++ builtins.filter (c: !(builtins.any (matches c) orig.children)) "
        <> nixList (map newEntry (nub children))
        <> "; }"

applyChildChanges :: ChildChanges -> Text
applyChildChanges (ChildChanges updates removals) =
    entryMatching
        <> "let updates = "
        <> nixList (map updateEntry updates)
        <> "; apply = c: builtins.foldl' (entry: u: if matches entry u then entry // { ${kind entry} = entry.${kind entry} // u.${kind entry}; } else entry) c updates; in orig // { children = map apply (builtins.filter (c: !(builtins.any (matches c) "
        <> nixList (map (`nixEntry` []) removals)
        <> ")) orig.children); }"

entryMatching :: Text
entryMatching = "let kind = c: if c ? step then \"step\" else \"project\"; matches = c: r: r ? ${kind c} && r.${kind c}.id == c.${kind c}.id; in "

newEntry :: ChildRef -> Text
newEntry child = nixEntry child ["hidden = false;", "sortKey = null;"]

updateEntry :: ChildUpdate -> Text
updateEntry (ChildUpdate child hidden sortKey) =
    nixEntry child (catMaybes [nixField "hidden" . nixBool <$> hidden, nixField "sortKey" . maybe "null" nixInt <$> sortKey])
  where
    nixField name value = name <> " = " <> value <> ";"
    nixBool flag = if flag then "true" else "false"

nixEntry :: ChildRef -> [Text] -> Text
nixEntry child fields = "{ " <> childKind child <> " = { " <> T.unwords (("id = " <> nixInt (childId child) <> ";") : fields) <> " }; }"

nixInt :: Int -> Text
nixInt = T.pack . show

nixList :: [Text] -> Text
nixList items = "[ " <> T.concat (map (<> " ") items) <> "]"
