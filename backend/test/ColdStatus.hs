{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Handlers.Cold (ColdFile (..), ColdFileState (..), ColdStatus (..), parseColdStatus)

main :: IO ()
main = do
    assertEqual
        "each provider state"
        (Just (ColdStatus [ColdFile "a.csv" Offloaded, ColdFile "b.png" (Onloading 5 10), ColdFile "c.bam" (OnloadFailed "box unreachable")]))
        ( parseColdStatus
            "{\"files\":[{\"path\":\"a.csv\",\"state\":\"offloaded\"},{\"path\":\"b.png\",\"state\":\"onloading\",\"done\":5,\"total\":10},{\"path\":\"c.bam\",\"state\":\"failed\",\"error\":\"box unreachable\"}]}"
        )
    assertEqual "a local or untracked output" (Just (ColdStatus [])) (parseColdStatus "{\"files\":[]}")
    assertEqual "onloading without progress is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"b.png\",\"state\":\"onloading\"}]}")
    assertEqual "a failure without its error is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"c.bam\",\"state\":\"failed\"}]}")
    assertEqual "an unknown state is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"a.csv\",\"state\":\"frozen\"}]}")
    assertEqual "output that is not JSON" Nothing (parseColdStatus "not json")

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
