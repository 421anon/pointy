{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectTree (
    ChildRef (..),
    ProjectFields (..),
    TreeOp (..),
    TreeOpError (..),
    ProjectSource (..),
    TreeState (..),
    TreePlan (..),
    renderTreeOpError,
    describeTreeOps,
    newProject,
    normalizeProjects,
    normalizeProject,
    stepEntries,
    projectStepEntries,
    projectChildRefs,
    applyTreeOps,
) where

import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Pair, parseEither)
import Data.Char (toUpper)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Foldable (asum)
import Data.List (intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

data ChildRef
    = StepChild Int
    | ProjectChild Int
    deriving (Eq, Ord, Show)

instance FromJSON ChildRef where
    parseJSON value = either fail (pure . entryRef) (parseEntry value)

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

data TreeOp
    = TreeUpdate Int ProjectFields
    | TreeLink Int ChildRef
    | TreeUnlink Int ChildRef
    | TreeOrder Int [ChildRef]
    | TreeHide Int ChildRef Bool
    | TreeDelete ChildRef
    deriving (Eq, Show)

instance FromJSON TreeOp where
    parseJSON = withObject "TreeOp" $ \fields ->
        fields .: "op" >>= \case
            "update" -> TreeUpdate <$> fields .: "project" <*> fields .: "fields"
            "link" -> TreeLink <$> fields .: "parent" <*> fields .: "child"
            "unlink" -> TreeUnlink <$> fields .: "parent" <*> fields .: "child"
            "order" -> TreeOrder <$> fields .: "parent" <*> fields .: "children"
            "hide" -> TreeHide <$> fields .: "parent" <*> fields .: "child" <*> fields .: "hidden"
            "delete" -> TreeDelete <$> fields .: "child"
            other -> fail ("Unknown project tree operation: " ++ other)

data TreeOpError
    = UnknownChild ChildRef
    | UnreadableProjectFile Int String
    | LegacyProjectFile Int
    | DeletedEarlier ChildRef
    | RootProtected
    | CyclicLink [Int]
    deriving (Eq, Show)

renderTreeOpError :: TreeOpError -> String
renderTreeOpError = \case
    UnknownChild child -> capitalize (describeChild child) ++ " does not exist."
    UnreadableProjectFile projectId reason -> "projects/" ++ show projectId ++ ".nix cannot be read: " ++ reason
    LegacyProjectFile projectId -> "projects/" ++ show projectId ++ ".nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it."
    DeletedEarlier child -> capitalize (describeChild child) ++ " was deleted earlier in this batch."
    RootProtected -> "The root project 0 cannot become a child or be deleted."
    CyclicLink chain -> "That would introduce a project cycle: " ++ intercalate " -> " (map show chain) ++ "."

capitalize :: String -> String
capitalize (first : rest) = toUpper first : rest
capitalize [] = ""

describeTreeOps :: [TreeOp] -> String
describeTreeOps ops = case map describeOp ops of
    [] -> "Organize project tree"
    messages -> intercalate "; " messages

describeOp :: TreeOp -> String
describeOp = \case
    TreeUpdate projectId fields -> "Update project " ++ show projectId ++ ": " ++ T.unpack (projectName fields)
    TreeLink parent child -> "Add " ++ describeChild child ++ " to project " ++ show parent
    TreeUnlink parent child -> "Remove " ++ describeChild child ++ " from project " ++ show parent
    TreeOrder parent _ -> "Reorder children of project " ++ show parent
    TreeHide parent child hidden -> (if hidden then "Hide " else "Show ") ++ describeChild child ++ " in project " ++ show parent
    TreeDelete child -> "Delete " ++ describeChild child

data ProjectSource = ProjectReadable Object | ProjectUnreadable String
    deriving (Eq, Show)

data TreeState = TreeState
    { treeProjects :: Map Int ProjectSource
    , treeSteps :: Set Int
    }
    deriving (Eq, Show)

data TreePlan = TreePlan
    { planWrites :: Map Int Object
    , planDeletedProjects :: [Int]
    , planDeletedSteps :: [Int]
    , planChangedChildren :: Set Int
    }
    deriving (Eq, Show)

data Entry = Entry
    { entryRef :: ChildRef
    , entryInner :: Object
    , entryRaw :: Object
    }
    deriving (Eq, Show)

data FileChange = Unchanged | FieldsChanged | ChildrenChanged
    deriving (Eq, Ord, Show)

data ProjectFile = ProjectFile
    { fileObject :: Object
    , fileEntries :: [Entry]
    , fileChange :: FileChange
    }
    deriving (Eq, Show)

data ApplyState = ApplyState
    { stateFiles :: Map Int (Either TreeOpError ProjectFile)
    , stateSteps :: Set Int
    , stateDeleted :: Set ChildRef
    }

parseEntry :: Value -> Either String Entry
parseEntry (Object raw) = case (KeyMap.lookup "step" raw, KeyMap.lookup "project" raw) of
    (Just (Object inner), Nothing) -> build StepChild inner
    (Nothing, Just (Object inner)) -> build ProjectChild inner
    _ -> Left "expected exactly one of \"step\" and \"project\""
  where
    build tag inner = case parseEither (.: "id") inner of
        Right entryId -> Right (Entry (tag entryId) inner raw)
        Left err -> Left err
parseEntry _ = Left "a child entry is not an attribute set"

entryKey :: Entry -> Key.Key
entryKey = Key.fromText . childKind . entryRef

entrySortKey :: Entry -> Maybe Int
entrySortKey entry = case KeyMap.lookup "sortKey" (entryInner entry) of
    Just (Number number) -> toBoundedInteger number
    _ -> Nothing

data EntryPlacement = Placed Int Int | Unplaced Int
    deriving (Eq, Ord, Show)

entryPlacement :: Entry -> EntryPlacement
entryPlacement entry = case entrySortKey entry of
    Just sortKey -> Placed sortKey (childId (entryRef entry))
    Nothing -> Unplaced (childId (entryRef entry))

effectiveOrder :: [Entry] -> [Entry]
effectiveOrder = sortOn entryPlacement . nubOrdOn entryRef

projectChildRefs :: Object -> [ChildRef]
projectChildRefs project = case KeyMap.lookup "children" project of
    Just (Array items) -> map entryRef (effectiveOrder (mapMaybe (either (const Nothing) Just . parseEntry) (V.toList items)))
    _ -> []

renumberEntries :: [Entry] -> [Entry]
renumberEntries = zipWith number [0 :: Int ..]
  where
    number index entry = entry{entryInner = KeyMap.insert "sortKey" (toJSON index) (entryInner entry)}

rebuildEntry :: Entry -> Value
rebuildEntry entry = Object (KeyMap.insert (entryKey entry) (Object (entryInner entry)) (entryRaw entry))

childKind :: ChildRef -> Text
childKind (StepChild _) = "step"
childKind (ProjectChild _) = "project"

childId :: ChildRef -> Int
childId (StepChild stepId) = stepId
childId (ProjectChild projectId) = projectId

describeChild :: ChildRef -> String
describeChild child = T.unpack (childKind child) ++ " " ++ show (childId child)

newEntry :: ChildRef -> Entry
newEntry child = Entry child inner KeyMap.empty
  where
    inner = KeyMap.fromList [("id", toJSON (childId child)), ("hidden", Bool False), ("sortKey", Null)]

loadState :: Set Int -> Map Int ProjectSource -> ApplyState
loadState steps rawFiles =
    ApplyState
        { stateFiles = Map.mapWithKey loadFile rawFiles
        , stateSteps = steps
        , stateDeleted = Set.empty
        }

loadFile :: Int -> ProjectSource -> Either TreeOpError ProjectFile
loadFile projectId source = case source of
    ProjectUnreadable reason -> Left (UnreadableProjectFile projectId reason)
    ProjectReadable fields
        | isJust (legacySteps (Object fields)) -> Left (LegacyProjectFile projectId)
        | otherwise -> case KeyMap.lookup "children" fields of
            Just (Array items) -> case traverse parseEntry (V.toList items) of
                Left err -> Left (UnreadableProjectFile projectId err)
                Right entries ->
                    Right
                        ProjectFile
                            { fileObject = fields
                            , fileEntries = effectiveOrder entries
                            , fileChange = Unchanged
                            }
            Just _ -> Left (UnreadableProjectFile projectId "the children attribute is not a list")
            Nothing -> Left (UnreadableProjectFile projectId "the file has no children attribute")

projectFile :: Int -> ApplyState -> Either TreeOpError ProjectFile
projectFile projectId state
    | Set.member (ProjectChild projectId) (stateDeleted state) = Left (DeletedEarlier (ProjectChild projectId))
    | otherwise = case Map.lookup projectId (stateFiles state) of
        Nothing -> Left (UnknownChild (ProjectChild projectId))
        Just (Left err) -> Left err
        Just (Right file) -> Right file

editEntries :: Int -> ([Entry] -> [Entry]) -> ApplyState -> Either TreeOpError ApplyState
editEntries projectId change state = do
    file <- projectFile projectId state
    let entries = change (fileEntries file)
    pure $
        if entries == fileEntries file
            then state
            else state{stateFiles = Map.insert projectId (Right (setChildren entries file)) (stateFiles state)}

editObject :: Int -> (Object -> Object) -> ApplyState -> Either TreeOpError ApplyState
editObject projectId change state = do
    file <- projectFile projectId state
    let newObject = change (fileObject file)
        fileChange' = if newObject == fileObject file then fileChange file else max (fileChange file) FieldsChanged
    pure state{stateFiles = Map.insert projectId (Right file{fileObject = newObject, fileChange = fileChange'}) (stateFiles state)}

setChildren :: [Entry] -> ProjectFile -> ProjectFile
setChildren entries file = file{fileEntries = entries, fileChange = max (fileChange file) ChildrenChanged}

childExists :: ChildRef -> ApplyState -> Either TreeOpError ()
childExists child state
    | Set.member child (stateDeleted state) = Left (DeletedEarlier child)
    | otherwise = case child of
        StepChild stepId
            | Set.member stepId (stateSteps state) -> Right ()
            | otherwise -> Left (UnknownChild (StepChild stepId))
        ProjectChild projectId
            | Map.member projectId (stateFiles state) -> Right ()
            | otherwise -> Left (UnknownChild (ProjectChild projectId))

projectChildrenOf :: ApplyState -> Int -> [Int]
projectChildrenOf state projectId = case Map.lookup projectId (stateFiles state) of
    Just (Right file) -> [pid | ProjectChild pid <- map entryRef (fileEntries file)]
    _ -> []

cycleExistedInitially :: ApplyState -> [Int] -> Bool
cycleExistedInitially initial chain = all edgePresent (zip chain (drop 1 chain))
  where
    edgePresent (from, to) = to `elem` projectChildrenOf initial from

linkCycle :: ApplyState -> Int -> Int -> Maybe [Int]
linkCycle state parent child = fmap (parent :) (go Set.empty [] child)
  where
    go visited path node
        | node == parent = Just (reverse (node : path))
        | Set.member node visited = Nothing
        | otherwise = asum [go (Set.insert node visited) (node : path) next | next <- projectChildrenOf state node]

orderEntries :: [ChildRef] -> [Entry] -> [Entry]
orderEntries listed entries = front ++ back
  where
    chosen = Set.fromList listed
    front = [entry | ref <- nubOrd listed, entry <- entries, entryRef entry == ref]
    back = [entry | entry <- entries, Set.notMember (entryRef entry) chosen]

replaceFields :: ProjectFields -> Object -> Object
replaceFields fields = KeyMap.union (KeyMap.fromList (projectFieldPairs fields)) . KeyMap.delete "templates" . KeyMap.delete "preset"

setHiddenEntry :: Bool -> Entry -> Entry
setHiddenEntry hidden entry = entry{entryInner = KeyMap.insert "hidden" (Bool hidden) (entryInner entry)}

applyOp :: ApplyState -> ApplyState -> TreeOp -> Either TreeOpError ApplyState
applyOp initial current = \case
    TreeUpdate projectId fields -> editObject projectId (replaceFields fields) current
    TreeLink parent child -> do
        parentFile <- projectFile parent current
        childExists child current
        if any ((== child) . entryRef) (fileEntries parentFile)
            then Right current
            else do
                case child of
                    ProjectChild childProject
                        | childProject == 0 -> Left RootProtected
                        | otherwise -> case linkCycle current parent childProject of
                            Just chain
                                | not (cycleExistedInitially initial chain) -> Left (CyclicLink chain)
                            _ -> pure ()
                    StepChild _ -> pure ()
                editEntries parent (++ [newEntry child]) current
    TreeUnlink parent child -> editEntries parent (filter ((/= child) . entryRef)) current
    TreeOrder parent children -> editEntries parent (orderEntries children) current
    TreeHide parent child hidden -> editEntries parent (map (hideOf child hidden)) current
    TreeDelete child -> do
        case child of
            ProjectChild 0 -> Left RootProtected
            _ -> Right ()
        case [err | Left err <- Map.elems (stateFiles current)] of
            err : _ -> Left err
            [] -> childExists child current
        let unlinkFrom current' parentId = editEntries parentId (filter ((/= child) . entryRef)) current'
        unlinked <- foldM unlinkFrom current (parentsOf child current)
        pure $ case child of
            ProjectChild projectId ->
                unlinked{stateFiles = Map.delete projectId (stateFiles unlinked), stateDeleted = Set.insert child (stateDeleted unlinked)}
            StepChild stepId ->
                unlinked{stateSteps = Set.delete stepId (stateSteps unlinked), stateDeleted = Set.insert child (stateDeleted unlinked)}
  where
    hideOf child hidden entry = if entryRef entry == child then setHiddenEntry hidden entry else entry
    parentsOf child state = [parentId | (parentId, Right file) <- Map.toList (stateFiles state), any ((== child) . entryRef) (fileEntries file)]

applyTreeOps :: TreeState -> [TreeOp] -> Either TreeOpError TreePlan
applyTreeOps state ops = do
    let initial = loadState (treeSteps state) (treeProjects state)
    final <- foldM (applyOp initial) initial ops
    let files = [(projectId, file) | (projectId, Right file) <- Map.toList (stateFiles final)]
        deleted = Set.toList (stateDeleted final)
    pure
        TreePlan
            { planWrites = Map.fromList [(projectId, written file) | (projectId, file) <- files, fileChange file /= Unchanged]
            , planDeletedProjects = [projectId | ProjectChild projectId <- deleted]
            , planDeletedSteps = [stepId | StepChild stepId <- deleted]
            , planChangedChildren = Set.fromList [projectId | (projectId, file) <- files, fileChange file == ChildrenChanged]
            }
  where
    written file
        | fileChange file == ChildrenChanged = KeyMap.insert "children" (Array (V.fromList (map rebuildEntry (renumberEntries (fileEntries file))))) (fileObject file)
        | otherwise = fileObject file

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
