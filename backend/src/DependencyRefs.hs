{-# LANGUAGE OverloadedStrings #-}

module DependencyRefs (
    ReferenceRelocation (..),
    stepRelocations,
    rewriteDependencyRefs,
    stripReferencePaths,
    flakeStepDefinitionsExpression,
    decodeStepDefinition,
    decodeStepDefinitions,
) where

import Data.Aeson (Object, Value (..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import ProjectTree (ChildRef (..), projectChildRefs, projectIds, projectObject)
import Text.Read (readMaybe)

stepFolderPathLists :: Value -> Map Int [[Int]]
stepFolderPathLists projects = Map.map nubOrd (Map.fromListWith (flip (++)) (concatMap (walk Set.empty []) walkRoots))
  where
    ids = projectIds projects
    childRefsOf projectId = maybe [] projectChildRefs (projectObject projects projectId)
    nestedIds = Set.fromList [nested | projectId <- ids, ProjectChild nested <- childRefsOf projectId]
    roots = filter (`Set.notMember` nestedIds) ids
    walkRoots = case projectObject projects 0 of
        Just _ -> [0]
        Nothing -> roots
    walk visited ancestors projectId =
        let path = if projectId == 0 then [] else ancestors ++ [projectId]
            children = childRefsOf projectId
            visited' = Set.insert projectId visited
            here = [(stepId, [path]) | StepChild stepId <- children]
            nested = [walk visited' path child | ProjectChild child <- children, Set.notMember child visited']
         in here ++ concat nested

data ReferenceRelocation = ReferenceRelocation
    { relocationLostPath :: [Int]
    , relocationNewPath :: Maybe [Int]
    }
    deriving (Eq, Show)

stepRelocations :: Value -> Value -> Map Int [ReferenceRelocation]
stepRelocations before after =
    Map.mapMaybeWithKey relocationsOf (Map.union beforePaths afterPaths)
  where
    beforePaths = stepFolderPathLists before
    afterPaths = stepFolderPathLists after
    relocationsOf stepId _ =
        case lostPaths of
            [] -> Nothing
            lost -> Just [ReferenceRelocation path (singleGain path) | path <- lost]
      where
        beforePathsOfStep = Map.findWithDefault [] stepId beforePaths
        afterPathsOfStep = Map.findWithDefault [] stepId afterPaths
        lostPaths = filter (`notElem` afterPathsOfStep) beforePathsOfStep
        gainedPaths = filter (`notElem` beforePathsOfStep) afterPathsOfStep
        singleGain _ = case gainedPaths of
            [gained] -> Just gained
            _ -> Nothing

rewriteDependencyRefs :: Map Int [ReferenceRelocation] -> Value -> (Bool, Value)
rewriteDependencyRefs relocations = rewrite
  where
    rewrite (Object fields) =
        let (childChanged, rewrittenFields) = mapAccumKeyMap rewrite fields
            (referenceChanged, rewrittenReference) = relocate rewrittenFields
         in (childChanged || referenceChanged, Object rewrittenReference)
    rewrite (Array items) =
        let (changed, rewrittenItems) = mapAccumVector rewrite items
         in (changed, Array rewrittenItems)
    rewrite value = (False, value)

    relocate fields = case (stepIdOf fields, referencePathOf fields) of
        (Just stepId, Just path)
            | Just pathRelocations <- Map.lookup stepId relocations
            , Just relocation <- firstMatch path pathRelocations ->
                case relocationNewPath relocation of
                    Just newPath -> (True, KeyMap.insert "path" (toJSONPath newPath) fields)
                    Nothing -> (True, KeyMap.delete "path" fields)
        _ -> (False, fields)

    firstMatch _ [] = Nothing
    firstMatch path (relocation : rest)
        | relocationLostPath relocation == path = Just relocation
        | otherwise = firstMatch path rest

mapAccumKeyMap :: (Value -> (Bool, Value)) -> Object -> (Bool, Object)
mapAccumKeyMap change = foldr step (False, KeyMap.empty) . KeyMap.toList
  where
    step (key, value) (changed, accumulated) =
        let (valueChanged, rewritten) = change value
         in (changed || valueChanged, KeyMap.insert key rewritten accumulated)

mapAccumVector :: (Value -> (Bool, Value)) -> V.Vector Value -> (Bool, V.Vector Value)
mapAccumVector change = V.foldr step (False, V.empty)
  where
    step value (changed, accumulated) =
        let (valueChanged, rewritten) = change value
         in (changed || valueChanged, V.cons rewritten accumulated)

stepIdOf :: Object -> Maybe Int
stepIdOf fields = case KeyMap.lookup "step" fields of
    Just (Number stepId) -> toBoundedInteger stepId
    _ -> Nothing

referencePathOf :: Object -> Maybe [Int]
referencePathOf fields = case KeyMap.lookup "path" fields of
    Just (Array items) -> traverse pathSegment (V.toList items)
    _ -> Nothing
  where
    pathSegment (Number segment) = toBoundedInteger segment
    pathSegment _ = Nothing

toJSONPath :: [Int] -> Value
toJSONPath path = Array (V.fromList (map (Number . fromIntegral) path))

stripReferencePaths :: Value -> Value
stripReferencePaths (Object fields)
    | KeyMap.member "step" fields =
        Object (KeyMap.map stripReferencePaths (KeyMap.delete "path" fields))
    | otherwise = Object (KeyMap.map stripReferencePaths fields)
stripReferencePaths (Array items) = Array (fmap stripReferencePaths items)
stripReferencePaths value = value

flakeStepDefinitionsExpression :: [Int] -> String
flakeStepDefinitionsExpression stepIds =
    "flake: let dir = flake.outPath + \"/steps\"; "
        ++ "load = id: let path = dir + (\"/\" + toString id + \".nix\"); "
        ++ "in { name = toString id; value = if builtins.pathExists path then let attempt = builtins.tryEval (builtins.deepSeq (import path) (import path)); in if attempt.success then attempt.value else null else null; }; "
        ++ "in builtins.listToAttrs (map load [ "
        ++ unwords (map show stepIds)
        ++ " ])"

decodeStepDefinition :: String -> Either String (Maybe Value)
decodeStepDefinition output = case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
    Left err -> Left ("Failed to decode a step definition: " ++ err)
    Right Null -> Right Nothing
    Right definition -> Right (Just definition)

decodeStepDefinitions :: String -> Either String (Map Int (Maybe Value))
decodeStepDefinitions output = case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
    Left err -> Left ("Failed to decode step definitions: " ++ err)
    Right (Object entries) -> Map.fromList <$> traverse entry (KeyMap.toList entries)
    Right _ -> Left "The step definitions are not an object."
  where
    entry (key, value) = case readMaybe (T.unpack (Key.toText key)) of
        Nothing -> Left ("Unexpected step definition key: " ++ T.unpack (Key.toText key))
        Just stepId -> Right (stepId, if value == Null then Nothing else Just value)
