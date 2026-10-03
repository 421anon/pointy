{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecode, fromJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import ProjectTree (
    ChildRef (..),
    ProjectFields (..),
    ProjectSource (..),
    TreeOp (..),
    TreeOpError (..),
    TreePlan (..),
    TreeState (..),
    applyTreeOps,
    normalizeProjects,
    renderTreeOpError,
 )

main :: IO ()
main = do
    normalizerTests
    childRefTests
    decodeTests
    applyTests

normalizerTests :: IO ()
normalizerTests = do
    assertEqual
        "projects in the steps format move their steps into children and a synthesized root lists them in ascending id with their former placement"
        convertedProjects
        (normalizeProjects legacyProjects)
    assertEqual "projects in the children format are unchanged" currentProjects (normalizeProjects currentProjects)

childRefTests :: IO ()
childRefTests = do
    assertEqual "a step reference" (Right (StepChild 156)) (parse "{\"step\":{\"id\":156}}")
    assertEqual "a project reference" (Right (ProjectChild 9)) (parse "{\"project\":{\"id\":9}}")
    assertBool "a reference names exactly one kind of child" (isLeft (parse "{\"step\":{\"id\":1},\"project\":{\"id\":2}}" :: Either String ChildRef))

decodeTests :: IO ()
decodeTests = do
    assertEqual "an update op decodes its project and fields" (Right (TreeUpdate 3 (ProjectFields "N" (Just "p") Nothing))) (parse "{\"op\":\"update\",\"project\":3,\"fields\":{\"name\":\"N\",\"preset\":\"p\"}}")
    assertEqual "an update op decodes templates" (Right (TreeUpdate 3 (ProjectFields "N" Nothing (Just ["t"])))) (parse "{\"op\":\"update\",\"project\":3,\"fields\":{\"name\":\"N\",\"templates\":[\"t\"]}}")
    assertEqual "a link op decodes its parent and child" (Right (TreeLink 1 (StepChild 5))) (parse "{\"op\":\"link\",\"parent\":1,\"child\":{\"step\":{\"id\":5}}}")
    assertEqual "an unlink op decodes its parent and child" (Right (TreeUnlink 1 (ProjectChild 2))) (parse "{\"op\":\"unlink\",\"parent\":1,\"child\":{\"project\":{\"id\":2}}}")
    assertEqual "an order op decodes its listed children" (Right (TreeOrder 1 [ProjectChild 2, StepChild 5])) (parse "{\"op\":\"order\",\"parent\":1,\"children\":[{\"project\":{\"id\":2}},{\"step\":{\"id\":5}}]}")
    assertEqual "a hide op decodes its flag" (Right (TreeHide 1 (StepChild 5) True)) (parse "{\"op\":\"hide\",\"parent\":1,\"child\":{\"step\":{\"id\":5}},\"hidden\":true}")
    assertEqual "a delete op decodes its child" (Right (TreeDelete (StepChild 5))) (parse "{\"op\":\"delete\",\"child\":{\"step\":{\"id\":5}}}")
    assertBool "an unknown op is rejected" (isLeft (parse "{\"op\":\"frobnicate\"}" :: Either String TreeOp))

applyTests :: IO ()
applyTests = do
    orderTests
    renumberTests
    linkTests
    unlinkTests
    hideTests
    cycleTests
    rootTests
    deleteTests
    updateTests
    existenceTests
    legacyTests

orderTests :: IO ()
orderTests = do
    let plan =
            expectPlan "order with unlisted children" $
                applyTreeOps
                    ( projectState
                        [ (1, [childList [stepEntry 5 ["sortKey" .= (2 :: Int)], stepEntry 6 ["sortKey" .= (0 :: Int)], stepEntry 7 [], stepEntry 8 ["sortKey" .= (1 :: Int)]]])
                        ]
                        (Set.fromList [5, 6, 7, 8])
                    )
                    [TreeOrder 1 [StepChild 7, StepChild 5]]
    assertEqual
        "listed children come first in the given order and unlisted children keep their effective order after them"
        [StepChild 7, StepChild 5, StepChild 6, StepChild 8]
        (writtenRefs plan 1)

renumberTests :: IO ()
renumberTests = do
    let plan =
            expectPlan "dense renumbering" $
                applyTreeOps
                    ( projectState
                        [ (1, [childList [entryWithNote 5 ["hidden" .= True, "sortKey" .= (5 :: Int), "colour" .= ("red" :: String)], stepEntry 6 ["hidden" .= False, "sortKey" .= (0 :: Int)]]])
                        ]
                        (Set.fromList [5, 6])
                    )
                    [TreeOrder 1 [StepChild 5]]
        inners = entryInners plan 1
    assertEqual "children are rewritten in effective order" [StepChild 5, StepChild 6] (writtenRefs plan 1)
    assertEqual "sortKeys are renumbered densely from zero" [Just (Number 0), Just (Number 1)] (map (KeyMap.lookup "sortKey") inners)
    assertEqual "hidden flags survive renumbering" [Just (Bool True), Just (Bool False)] (map (KeyMap.lookup "hidden") inners)
    assertEqual "unknown inner attributes survive renumbering" (Just (String "red")) (KeyMap.lookup "colour" (at 0 inners))
    assertEqual "unknown entry attributes survive renumbering" (Just (String "keep")) (KeyMap.lookup "note" (at 0 (writtenRawEntries plan 1)))

linkTests :: IO ()
linkTests = do
    let idempotent =
            expectPlan "link idempotence" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 []]])] (Set.fromList [5, 6]))
                    [TreeLink 1 (StepChild 5)]
    assertBool "linking an already-linked child rewrites no project file" (Map.null (planWrites idempotent))
    assertBool "linking an already-linked child broadcasts no child change" (Set.null (planChangedChildren idempotent))

    let appended =
            expectPlan "link append" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 []]])] (Set.fromList [5, 6]))
                    [TreeLink 1 (StepChild 6)]
    assertEqual "a new link is appended after the existing children" [StepChild 5, StepChild 6] (writtenRefs appended 1)
    assertEqual "the appended link starts visible" (Just (Bool False)) (KeyMap.lookup "hidden" (at 1 (entryInners appended 1)))

    let deduped =
            expectPlan "duplicate links dedupe" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 ["sortKey" .= (0 :: Int)], stepEntry 5 ["sortKey" .= (1 :: Int)]]])] (Set.fromList [5, 6]))
                    [TreeLink 1 (StepChild 6)]
    assertEqual "duplicate links collapse to the first occurrence" [StepChild 5, StepChild 6] (writtenRefs deduped 1)

unlinkTests :: IO ()
unlinkTests = do
    let plan =
            expectPlan "unlink removes the matching entry" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 [], stepEntry 6 []]])] (Set.fromList [5, 6]))
                    [TreeUnlink 1 (StepChild 5)]
    assertEqual "the unlinked child is gone" [StepChild 6] (writtenRefs plan 1)
    assertEqual "the parent's children changed" (Set.fromList [1]) (planChangedChildren plan)

    let absent =
            expectPlan "unlinking an absent child does nothing" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 []]])] (Set.fromList [5]))
                    [TreeUnlink 1 (StepChild 6)]
    assertBool "an absent unlink rewrites no project file" (Map.null (planWrites absent))

hideTests :: IO ()
hideTests = do
    let plan =
            expectPlan "hide flips the matching entry" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 ["hidden" .= False, "sortKey" .= (0 :: Int)], stepEntry 6 ["hidden" .= False, "sortKey" .= (1 :: Int)]]])] (Set.fromList [5, 6]))
                    [TreeHide 1 (StepChild 5) True]
    assertEqual "the matching entry is hidden" (Just (Bool True)) (KeyMap.lookup "hidden" (at 0 (entryInners plan 1)))
    assertEqual "the other entry keeps its flag" (Just (Bool False)) (KeyMap.lookup "hidden" (at 1 (entryInners plan 1)))

    let absent =
            expectPlan "hiding an absent child does nothing" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 ["hidden" .= False]]])] (Set.fromList [5]))
                    [TreeHide 1 (StepChild 6) True]
    assertBool "an absent hide rewrites no project file" (Map.null (planWrites absent))

cycleTests :: IO ()
cycleTests = do
    expectError "a link that closes a cycle is rejected" (CyclicLink [1, 2, 1]) $
        applyTreeOps
            (projectState [(1, [childList []]), (2, [childList [projectEntry 1 []]])] Set.empty)
            [TreeLink 1 (ProjectChild 2)]

    let tolerated =
            expectPlan "a pre-existing cycle does not block other links" $
                applyTreeOps
                    (projectState [(1, [childList []]), (2, [childList [projectEntry 2 []]]), (3, [childList []])] Set.empty)
                    [TreeLink 1 (ProjectChild 3), TreeLink 3 (ProjectChild 2)]
    assertEqual "a link away from a pre-existing cycle is accepted" [ProjectChild 3] (writtenRefs tolerated 1)
    assertEqual "a project in a pre-existing cycle may still be linked as a child" [ProjectChild 2] (writtenRefs tolerated 3)

    let redundant =
            expectPlan "a redundant link inside a pre-existing cycle is a no-op" $
                applyTreeOps
                    (projectState [(1, [childList [projectEntry 2 []]]), (2, [childList [projectEntry 1 []]])] Set.empty)
                    [TreeLink 1 (ProjectChild 2)]
    assertBool "the redundant link rewrites no project file" (Map.null (planWrites redundant))
    assertBool "the redundant link broadcasts no child change" (Set.null (planChangedChildren redundant))

    let undone =
            expectPlan "restoring a link that closed a tolerated cycle is accepted" $
                applyTreeOps
                    (projectState [(1, [childList [projectEntry 2 []]]), (2, [childList [projectEntry 1 []]])] Set.empty)
                    [TreeUnlink 1 (ProjectChild 2), TreeLink 1 (ProjectChild 2)]
    assertEqual "the tolerated link is restored" [ProjectChild 2] (writtenRefs undone 1)

    expectError "a cycle closed through a newly added link is rejected" (CyclicLink [1, 2, 3, 1]) $
        applyTreeOps
            ( projectState
                [ (1, [childList [projectEntry 2 []]])
                , (2, [childList []])
                , (3, [childList [projectEntry 1 []]])
                ]
                Set.empty
            )
            [TreeUnlink 1 (ProjectChild 2), TreeLink 2 (ProjectChild 3), TreeLink 1 (ProjectChild 2)]

rootTests :: IO ()
rootTests = do
    expectError "the root project cannot become a child" RootProtected $
        applyTreeOps (projectState [(0, [childList []]), (1, [childList []])] Set.empty) [TreeLink 1 (ProjectChild 0)]
    expectError "the root project cannot be deleted" RootProtected $
        applyTreeOps (projectState [(0, [childList []])] Set.empty) [TreeDelete (ProjectChild 0)]

deleteTests :: IO ()
deleteTests = do
    let plan =
            expectPlan "delete unlinks everywhere" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 [], stepEntry 6 []]]), (2, [childList [stepEntry 5 []]])] (Set.fromList [5, 6]))
                    [TreeDelete (StepChild 5)]
    assertEqual "the deleted step is reported" [5] (planDeletedSteps plan)
    assertEqual "the first parent loses the link" [StepChild 6] (writtenRefs plan 1)
    assertBool "a parent left empty is rewritten" (Map.member 2 (planWrites plan))
    assertEqual "a parent left empty writes an empty child list" [] (writtenRefs plan 2)
    assertEqual "both parents are broadcast" (Set.fromList [1, 2]) (planChangedChildren plan)

    let projectPlan =
            expectPlan "deleting a project unlinks it everywhere" $
                applyTreeOps
                    (projectState [(1, [childList [projectEntry 2 [], projectEntry 3 []]]), (2, [childList []])] Set.empty)
                    [TreeDelete (ProjectChild 2)]
    assertEqual "the deleted project is reported" [2] (planDeletedProjects projectPlan)
    assertEqual "a surviving parent loses the project link" [ProjectChild 3] (writtenRefs projectPlan 1)

    expectError "a child deleted earlier in the batch cannot be linked again" (DeletedEarlier (StepChild 5)) $
        applyTreeOps
            (projectState [(1, [childList [stepEntry 5 []]])] (Set.fromList [5]))
            [TreeDelete (StepChild 5), TreeLink 1 (StepChild 5)]

    expectUnreadable "a delete is refused while an unreadable project file exists" 2 $
        applyTreeOps
            ( TreeState
                (Map.fromList [(1, ProjectReadable (objectOf (project 1 [childList [stepEntry 5 []]]))), (2, ProjectUnreadable "boom")])
                (Set.fromList [5])
            )
            [TreeDelete (StepChild 5)]

    expectError "a delete is refused while a legacy project file exists" (LegacyProjectFile 2) $
        applyTreeOps
            ( TreeState
                (Map.fromList [(1, ProjectReadable (objectOf (project 1 [childList [stepEntry 5 []]]))), (2, ProjectReadable (objectOf (object ["name" .= ("Legacy" :: String), "steps" .= ([] :: [Value])])))])
                (Set.fromList [5])
            )
            [TreeDelete (StepChild 5)]

updateTests :: IO ()
updateTests = do
    let plan =
            expectPlan "update keeps children" $
                applyTreeOps
                    (projectState [(1, [childList [stepEntry 5 []]])] (Set.fromList [5]))
                    [TreeUpdate 1 (ProjectFields "New" (Just "preset") Nothing)]
    assertEqual "children survive a field update" [StepChild 5] (writtenRefs plan 1)
    assertEqual "the name is replaced" (Just (String "New")) (Map.lookup 1 (planWrites plan) >>= KeyMap.lookup "name")
    assertEqual "the preset is written" (Just (String "preset")) (Map.lookup 1 (planWrites plan) >>= KeyMap.lookup "preset")

    let unchanged =
            expectPlan "an identical update rewrites nothing" $
                applyTreeOps
                    (projectState [(1, [childList []])] Set.empty)
                    [TreeUpdate 1 (ProjectFields "Project 1" Nothing Nothing)]
    assertBool "an identical update produces no write" (Map.null (planWrites unchanged))

existenceTests :: IO ()
existenceTests = do
    expectError "linking under an unknown parent is refused" (UnknownChild (ProjectChild 7)) $
        applyTreeOps (projectState [(1, [childList []])] (Set.fromList [5])) [TreeLink 7 (StepChild 5)]
    expectError "linking an unknown step is refused" (UnknownChild (StepChild 9)) $
        applyTreeOps (projectState [(1, [childList []])] (Set.fromList [5])) [TreeLink 1 (StepChild 9)]
    expectError "linking an unknown project is refused" (UnknownChild (ProjectChild 9)) $
        applyTreeOps (projectState [(1, [childList []])] Set.empty) [TreeLink 1 (ProjectChild 9)]

legacyTests :: IO ()
legacyTests = do
    let legacy =
            TreeState
                { treeProjects =
                    Map.fromList
                        [ (1, ProjectReadable (objectOf (object ["name" .= ("Legacy" :: String), "steps" .= [object ["def" .= object ["id" .= (5 :: Int)]]]])))
                        , (2, ProjectReadable (objectOf (project 2 [childList []])))
                        ]
                , treeSteps = Set.fromList [5]
                }
    expectError "a legacy project file refuses writes" (LegacyProjectFile 1) $
        applyTreeOps legacy [TreeLink 1 (StepChild 5)]
    let unrelated = expectPlan "an unrelated write ignores a legacy file" $ applyTreeOps legacy [TreeLink 2 (StepChild 5)]
    assertEqual "the readable project is still rewritten" [StepChild 5] (writtenRefs unrelated 2)

    expectError "an unreadable project file refuses writes" (UnreadableProjectFile 2 "boom") $
        applyTreeOps (TreeState (Map.fromList [(2, ProjectUnreadable "boom")]) Set.empty) [TreeUpdate 2 (ProjectFields "N" Nothing Nothing)]
    expectUnreadable "a project file without children is refused" 2 $
        applyTreeOps (TreeState (Map.fromList [(2, ProjectReadable (objectOf (object ["name" .= ("X" :: String)])))]) Set.empty) [TreeUpdate 2 (ProjectFields "N" Nothing Nothing)]

projectState :: [(Int, [Pair])] -> Set.Set Int -> TreeState
projectState projects steps =
    TreeState
        { treeProjects = Map.fromList [(projectId, ProjectReadable (objectOf (project projectId fields))) | (projectId, fields) <- projects]
        , treeSteps = steps
        }

project :: Int -> [Pair] -> Value
project projectId fields = object (("name" .= ("Project " ++ show projectId)) : fields)

childList :: [Value] -> Pair
childList = ("children" .=)

stepEntry :: Int -> [Pair] -> Value
stepEntry stepId extras = object ["step" .= object (("id" .= stepId) : extras)]

projectEntry :: Int -> [Pair] -> Value
projectEntry projectId extras = object ["project" .= object (("id" .= projectId) : extras)]

entryWithNote :: Int -> [Pair] -> Value
entryWithNote stepId extras = object ["note" .= ("keep" :: String), "step" .= object (("id" .= stepId) : extras)]

objectOf :: Value -> Object
objectOf (Object value) = value
objectOf _ = error "expected a JSON object"

at :: Int -> [a] -> a
at index values = case drop index values of
    (value : _) -> value
    [] -> error ("missing entry " ++ show index)

expectPlan :: String -> Either TreeOpError TreePlan -> TreePlan
expectPlan label = either (error . ((label ++ ": ") ++) . renderTreeOpError) id

expectError :: (Eq e, Show e) => String -> e -> Either e a -> IO ()
expectError label expected result = case result of
    Left err -> assertEqual label expected err
    Right _ -> fail (label ++ ": expected " ++ show expected ++ ", got a successful plan")

expectUnreadable :: String -> Int -> Either TreeOpError a -> IO ()
expectUnreadable label projectId result = case result of
    Left (UnreadableProjectFile actual _) -> assertEqual label projectId actual
    Left err -> fail (label ++ ": expected an unreadable project file, got " ++ show err)
    Right _ -> fail (label ++ ": expected an unreadable project file, got a successful plan")

writtenRawEntries :: TreePlan -> Int -> [Object]
writtenRawEntries plan projectId = case Map.lookup projectId (planWrites plan) >>= KeyMap.lookup "children" of
    Just (Array items) -> [raw | Object raw <- V.toList items]
    _ -> []

entryInners :: TreePlan -> Int -> [Object]
entryInners plan projectId =
    [inner | raw <- writtenRawEntries plan projectId, Just (Object inner) <- [KeyMap.lookup "step" raw <|> KeyMap.lookup "project" raw]]

writtenRefs :: TreePlan -> Int -> [ChildRef]
writtenRefs plan projectId = case Map.lookup projectId (planWrites plan) >>= KeyMap.lookup "children" of
    Just (Array items) -> case fromJSON (Array items) of
        Success refs -> refs
        Error err -> error err
    _ -> []

parse :: (FromJSON a) => LBS.ByteString -> Either String a
parse = eitherDecode

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual

legacyProjects :: Value
legacyProjects =
    object
        [ "2" .= project 2 ["hidden" .= False, "sortKey" .= Number 0, "steps" .= [legacyStep 156 False (Number 2), legacyStep 60 True Null]]
        , "3" .= project 3 ["steps" .= ([] :: [Value])]
        , "10" .= project 10 ["hidden" .= True, "sortKey" .= Null, "steps" .= ([] :: [Value])]
        ]

convertedProjects :: Value
convertedProjects =
    object
        [ "0"
            .= object
                [ "id" .= (0 :: Int)
                , "name" .= ("Home" :: String)
                , "preset" .= Null
                , "templates" .= ([] :: [String])
                , "validationErrors" .= ([] :: [String])
                , "children" .= [projectChild 2 False (Number 0), projectChild 3 False Null, projectChild 10 True Null]
                ]
        , "2" .= project 2 ["children" .= [stepChild 156 False (Number 2), stepChild 60 True Null]]
        , "3" .= project 3 ["children" .= ([] :: [Value])]
        , "10" .= project 10 ["children" .= ([] :: [Value])]
        ]

currentProjects :: Value
currentProjects =
    object
        [ "0" .= project 0 ["children" .= [projectChild 1 False Null]]
        , "1" .= project 1 ["children" .= [stepChild 156 False (Number 2), projectChild 1 True Null]]
        ]

legacyStep :: Int -> Bool -> Value -> Value
legacyStep stepId hidden sortKey = object ["def" .= object ["id" .= stepId], "hidden" .= hidden, "sortKey" .= sortKey]

stepChild :: Int -> Bool -> Value -> Value
stepChild stepId hidden sortKey = object ["step" .= object ["id" .= stepId, "def" .= object ["id" .= stepId], "hidden" .= hidden, "sortKey" .= sortKey]]

projectChild :: Int -> Bool -> Value -> Value
projectChild projectId hidden sortKey = object ["project" .= object ["id" .= projectId, "hidden" .= hidden, "sortKey" .= sortKey]]
