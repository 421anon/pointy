{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildRunner (BuildKey (..), JobComment (..), JobId (..), buildKeyForOutPath, decodeJobComment, encodeJobComment, parseSlurmJobLine, slurmJobComment, slurmJobId, slurmJobName, slurmJobState)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    -- squeue -o "%i|%j|%k|%T" line parsing
    let full =
            parseSlurmJobLine
                "12345|pointy-nix-build-nix-store-abc-step-7-0000000000000abc|{\"kind\":\"step\",\"step\":7,\"commit\":\"cafe\",\"outPath\":\"/nix/store/abc-step-7\"}|RUNNING"
    assertEqual "parses id" (Just "12345") (unJobId <$> (slurmJobId <$> full))
    assertEqual "parses name" (Just "pointy-nix-build-nix-store-abc-step-7-0000000000000abc") (slurmJobName <$> full)
    assertEqual "parses comment" (Just "{\"kind\":\"step\",\"step\":7,\"commit\":\"cafe\",\"outPath\":\"/nix/store/abc-step-7\"}") (slurmJobComment =<< full)
    assertEqual "parses state" (Just "RUNNING") (slurmJobState <$> full)

    let nullComment = parseSlurmJobLine "12346|pointy-nix-build-x-1|(null)|PENDING"
    assertEqual "null comment is Nothing" (Just Nothing) (slurmJobComment <$> nullComment)

    let emptyComment = parseSlurmJobLine "12347|pointy-nix-build-x-2||PENDING"
    assertEqual "empty comment is Nothing" (Just Nothing) (slurmJobComment <$> emptyComment)

    assertEqual "garbage line rejected" Nothing (parseSlurmJobLine "not a job line")
    assertEqual "missing state rejected" Nothing (parseSlurmJobLine "12348|pointy-nix-build-x-3|{}")

    -- comment round-trip
    let comment = JobComment "step" 7 "cafe" "/nix/store/abc-step-7"
        encoded = encodeJobComment comment
    assertEqual "comment round-trips" (Just comment) (decodeJobComment encoded)
    assertEqual "comment has no field separators" Nothing (lookup '|' [(c, ()) | c <- encoded])

    -- name validation: the name a restored job must carry for its outPath
    let name = buildKeyForOutPath "/nix/store/abc-step-7"
    assertEqual "name is deterministic" name (buildKeyForOutPath "/nix/store/abc-step-7")
    assertEqual "name embeds outPath" ("pointy-nix-build-" `isPrefixOf` unBuildKey name) True

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
