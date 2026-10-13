{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ArtifactRdf (ArtifactRef (..), closureTurtle, parseArtifactSegments, stepNodesFromClosure)
import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ProjectTree (stepFolderPaths)
import System.Exit (exitFailure)

main :: IO ()
main = do
    segmentTests
    closureTests

commit :: T.Text
commit = "fc87dfdf0123456789abcdef0123456789abcdef"

segmentTests :: IO ()
segmentTests = do
    assertEqual "a permalink names its project path, step and commit" (Just (ArtifactRef [113, 131, 134] 3294 commit)) (parseArtifactSegments ["113", "131", "134", "3294", commit])
    assertEqual "a permalink outside any folder has an empty project path" (Just (ArtifactRef [] 3294 commit)) (parseArtifactSegments ["3294", commit])
    assertEqual "a branch name is not an immutable commit" Nothing (parseArtifactSegments ["113", "3294", "main"])
    assertEqual "an abbreviated commit shorter than seven digits is rejected" Nothing (parseArtifactSegments ["3294", "fc87df"])
    assertEqual "a non-numeric step id is rejected" Nothing (parseArtifactSegments ["113", "x3294", commit])
    assertEqual "a trailing slash is rejected" Nothing (parseArtifactSegments ["113", "3294", commit, ""])

closureTests :: IO ()
closureTests = do
    let ref = ArtifactRef [113, 131, 134] 3294 commit
        rendered = fmap (closureTurtle ref (stepFolderPaths projects)) (stepNodesFromClosure closure)
    assertEqual "the closure renders the requested step first, upstream steps by id, edges within the closure and only http(s) about links" (Just expected) rendered

closure :: Value
closure =
    object
        [ "3294"
            .= entry
                (object ["name" .= ("Plot \"dead\"\nand ures" :: T.Text), "type" .= ("script" :: T.Text), "args" .= object ["stepDeps" .= [object ["step" .= (3058 :: Int)], object ["step" .= (999 :: Int)]], "body" .= ("python3 plot.py" :: T.Text)]])
                (Just "/nix/store/aaaa-out")
        , "3058"
            .= entry
                (object ["name" .= ("Fk3a_OP1s2_dead_sc1 merged" :: T.Text), "type" .= ("merge" :: T.Text), "args" .= object ["dataSource" .= object ["step" .= (3010 :: Int)]], "about" .= (["https://herbie.example.org/graph/samples/Q2x/", "javascript:alert(1)", "https://herbie.example.org/a b", "not a link"] :: [T.Text])])
                Nothing
        , "3010"
            .= entry
                (object ["name" .= ("" :: T.Text), "type" .= ("dataSource" :: T.Text), "args" .= object ["uploaded" .= object ["hash" .= ("sha256-x" :: T.Text)]]])
                (Just "/nix/store/bbbb-novogene")
        ]
  where
    entry def output = object ["def" .= def, "output" .= (output :: Maybe T.Text)]

projects :: Value
projects =
    object
        [ "0" .= object ["children" .= [projectChild 113, projectChild 200]]
        , "113" .= object ["name" .= ("NGS46" :: T.Text), "children" .= [stepChild 3058]]
        , "200" .= object ["name" .= ("Other" :: T.Text), "children" .= [stepChild 3058]]
        ]
  where
    projectChild projectId = object ["project" .= object ["id" .= (projectId :: Int)]]
    stepChild stepId = object ["step" .= object ["id" .= (stepId :: Int)]]

expected :: T.Text
expected =
    T.unlines
        [ "@prefix prov: <http://www.w3.org/ns/prov#> ."
        , "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
        , "@prefix schema: <https://schema.org/> ."
        , "@prefix pointy: <https://pointy.cloud/ns#> ."
        , ""
        , "</artifact/113/131/134/3294/" <> commit <> ">"
        , "    a prov:Plan ;"
        , "    rdfs:label \"Plot \\\"dead\\\"\\nand ures\" ;"
        , "    pointy:template \"script\" ;"
        , "    pointy:output \"/nix/store/aaaa-out\" ;"
        , "    prov:wasDerivedFrom </artifact/113/3058/" <> commit <> "> ."
        , ""
        , "</artifact/3010/" <> commit <> ">"
        , "    a prov:Plan ;"
        , "    rdfs:label \"3010\" ;"
        , "    pointy:template \"dataSource\" ;"
        , "    pointy:output \"/nix/store/bbbb-novogene\" ."
        , ""
        , "</artifact/113/3058/" <> commit <> ">"
        , "    a prov:Plan ;"
        , "    rdfs:label \"Fk3a_OP1s2_dead_sc1 merged\" ;"
        , "    pointy:template \"merge\" ;"
        , "    prov:wasDerivedFrom </artifact/3010/" <> commit <> "> ;"
        , "    schema:about <https://herbie.example.org/graph/samples/Q2x/> ."
        ]

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expectedValue actual =
    unless (expectedValue == actual) $ do
        TIO.putStrLn (T.pack ("FAIL: " ++ label))
        TIO.putStrLn (T.pack ("expected: " ++ show expectedValue))
        TIO.putStrLn (T.pack ("actual:   " ++ show actual))
        exitFailure
