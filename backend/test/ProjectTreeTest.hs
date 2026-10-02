{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON, Value (..), eitherDecode, object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import ProjectTree (ChildChanges (..), ChildRef (..), ChildUpdate (..), normalizeProjects)

main :: IO ()
main = do
    assertEqual
        "projects in the steps format move their steps into children and a synthesized root lists them in ascending id with their former placement"
        convertedProjects
        (normalizeProjects legacyProjects)
    assertEqual "projects in the children format are unchanged" currentProjects (normalizeProjects currentProjects)

    assertEqual "a step reference" (Right (StepChild 156)) (parse "{\"step\":{\"id\":156}}")
    assertEqual "a project reference" (Right (ProjectChild 9)) (parse "{\"project\":{\"id\":9}}")
    assertBool "a reference names exactly one kind of child" (isLeft (parse "{\"step\":{\"id\":1},\"project\":{\"id\":2}}" :: Either String ChildRef))
    assertEqual "an absent sortKey is left unchanged" (Right (ChildUpdate (StepChild 156) (Just True) Nothing)) (parse "{\"step\":{\"id\":156,\"hidden\":true}}")
    assertEqual "a null sortKey clears the sort key" (Right (ChildUpdate (ProjectChild 9) Nothing (Just Nothing))) (parse "{\"project\":{\"id\":9,\"sortKey\":null}}")
    assertEqual "a numeric sortKey sets the sort key" (Right (ChildUpdate (ProjectChild 9) Nothing (Just (Just 3)))) (parse "{\"project\":{\"id\":9,\"sortKey\":3}}")
    assertEqual
        "changes carry updates and removals"
        (Right (ChildChanges [ChildUpdate (StepChild 1) (Just False) Nothing] [ProjectChild 2]))
        (parse "{\"update\":[{\"step\":{\"id\":1,\"hidden\":false}}],\"remove\":[{\"project\":{\"id\":2}}]}")

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

project :: Int -> [Pair] -> Value
project projectId fields =
    object (["id" .= projectId, "name" .= ("Project " ++ show projectId), "preset" .= ("ngs-evaluation" :: String), "templates" .= Null, "validationErrors" .= ([] :: [String])] ++ fields)

legacyStep :: Int -> Bool -> Value -> Value
legacyStep stepId hidden sortKey = object ["def" .= object ["id" .= stepId], "hidden" .= hidden, "sortKey" .= sortKey]

stepChild :: Int -> Bool -> Value -> Value
stepChild stepId hidden sortKey = object ["step" .= object ["id" .= stepId, "def" .= object ["id" .= stepId], "hidden" .= hidden, "sortKey" .= sortKey]]

projectChild :: Int -> Bool -> Value -> Value
projectChild projectId hidden sortKey = object ["project" .= object ["id" .= projectId, "hidden" .= hidden, "sortKey" .= sortKey]]

parse :: (FromJSON a) => LBS.ByteString -> Either String a
parse = eitherDecode

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
