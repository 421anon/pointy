{-# LANGUAGE OverloadedStrings #-}

module ArtifactRdf (
    ArtifactRef (..),
    StepNode (..),
    parseArtifactSegments,
    artifactPath,
    closureExpression,
    stepNodesFromClosure,
    closureTurtle,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isHexDigit, ord)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.URI (parseAbsoluteURI, uriScheme)
import Numeric (showHex)
import Text.Read (readMaybe)

data ArtifactRef = ArtifactRef
    { refProjectPath :: [Int]
    , refStepId :: Int
    , refCommit :: Text
    }
    deriving (Eq, Show)

data StepNode = StepNode
    { nodeId :: Int
    , nodeName :: Text
    , nodeTemplate :: Text
    , nodeOutput :: Maybe Text
    , nodeAbout :: [Text]
    , nodeProducers :: [Int]
    }
    deriving (Eq, Show)

parseArtifactSegments :: [Text] -> Maybe ArtifactRef
parseArtifactSegments segments = case reverse segments of
    commit : stepSegment : reversedPath
        | isCommit commit -> do
            stepId <- readId stepSegment
            projectPath <- mapM readId (reverse reversedPath)
            pure (ArtifactRef projectPath stepId commit)
    _ -> Nothing
  where
    isCommit commit = T.length commit >= 7 && T.all (\c -> isHexDigit c && c `notElem` ['A' .. 'F']) commit
    readId segment
        | not (T.null segment) && T.all (`elem` ['0' .. '9']) segment = readMaybe (T.unpack segment)
        | otherwise = Nothing

artifactPath :: [Int] -> Int -> Text -> Text
artifactPath projectPath stepId commit =
    "/artifact/" <> T.intercalate "/" (map (T.pack . show) (projectPath ++ [stepId]) ++ [commit])

closureExpression :: Int -> String
closureExpression stepId =
    concat
        [ "steps: let root = "
        , show (show stepId)
        , "; output = id: let attempt = builtins.tryEval steps.${id}.outPath; in if attempt.success then attempt.value else null;"
        , " in if steps ? ${root} then builtins.listToAttrs (map (id: { name = id; value = { def = steps.${id}.def; output = output id; }; })"
        , " ([ root ] ++ map toString steps.${root}.dependencies)) else null"
        ]

stepNodesFromClosure :: Value -> Maybe [StepNode]
stepNodesFromClosure (Object entries) = Just (mapMaybe node (KeyMap.toList entries))
  where
    node (key, Object entry) = do
        stepId <- readMaybe (T.unpack (Key.toText key))
        Object def <- KeyMap.lookup "def" entry
        pure
            StepNode
                { nodeId = stepId
                , nodeName = case textField "name" def of
                    name | T.null (T.strip name) -> T.pack (show stepId)
                    name -> name
                , nodeTemplate = textField "type" def
                , nodeOutput = case KeyMap.lookup "output" entry of
                    Just (String path) -> Just path
                    _ -> Nothing
                , nodeAbout = case KeyMap.lookup "about" def of
                    Just (Array items) -> nubOrd [link | String link <- V.toList items, isLinkableIri link]
                    _ -> []
                , nodeProducers = maybe [] stepRefs (KeyMap.lookup "args" def)
                }
    node _ = Nothing
    textField key def = case KeyMap.lookup key def of
        Just (String value) -> value
        _ -> ""
stepNodesFromClosure _ = Nothing

stepRefs :: Value -> [Int]
stepRefs (Object fields) = case KeyMap.lookup "step" fields of
    Just (Number stepId) -> maybe [] pure (toBoundedInteger stepId)
    _ -> concatMap stepRefs (KeyMap.elems fields)
stepRefs (Array items) = concatMap stepRefs (V.toList items)
stepRefs _ = []

isLinkableIri :: Text -> Bool
isLinkableIri link =
    T.all allowed link && case parseAbsoluteURI (T.unpack link) of
        Just uri -> uriScheme uri `elem` ["http:", "https:"]
        Nothing -> False
  where
    allowed c = c > ' ' && c `notElem` ("<>\"{}|^`\\" :: String)

closureTurtle :: ArtifactRef -> Map Int [Int] -> [StepNode] -> Text
closureTurtle ref folders nodes =
    T.unlines (prefixes ++ concatMap description ordered)
  where
    root = refStepId ref
    members = Set.fromList (map nodeId nodes)
    ordered = [node | node <- nodes, nodeId node == root] ++ sortOn nodeId [node | node <- nodes, nodeId node /= root]
    iriOf stepId
        | stepId == root = iri (artifactPath (refProjectPath ref) stepId (refCommit ref))
        | otherwise = iri (artifactPath (Map.findWithDefault [] stepId folders) stepId (refCommit ref))
    description node =
        ["", iriOf (nodeId node)]
            ++ objectLines
                ( [("a", ["prov:Plan"])]
                    ++ [("rdfs:label", [literal (nodeName node)])]
                    ++ [("pointy:template", [literal (nodeTemplate node)]) | not (T.null (nodeTemplate node))]
                    ++ [("pointy:output", [literal output]) | Just output <- [nodeOutput node]]
                    ++ [("prov:wasDerivedFrom", map iriOf (producersOf node)) | not (null (producersOf node))]
                    ++ [("schema:about", map iri (nodeAbout node)) | not (null (nodeAbout node))]
                )
    producersOf node = sort (nubOrd (filter (\p -> p /= nodeId node && Set.member p members) (nodeProducers node)))

objectLines :: [(Text, [Text])] -> [Text]
objectLines predicates =
    zipWith line [1 :: Int ..] predicates
  where
    count = length predicates
    line index (predicate, objects) =
        "    " <> predicate <> " " <> T.intercalate ", " objects <> (if index == count then " ." else " ;")

prefixes :: [Text]
prefixes =
    [ "@prefix prov: <http://www.w3.org/ns/prov#> ."
    , "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
    , "@prefix schema: <https://schema.org/> ."
    , "@prefix pointy: <https://pointy.cloud/ns#> ."
    ]

iri :: Text -> Text
iri value = "<" <> value <> ">"

literal :: Text -> Text
literal value = "\"" <> T.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | c < ' ' || c == '\DEL' = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
        | otherwise = T.singleton c
