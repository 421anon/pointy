{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import Handlers.Cold (ColdFile (..), ColdStatus (..), PrefetchResponse (..), offloadedFilesNamed, parseColdFiles, parseColdStatus, parsePrefetchResponse, stepIdsNamedIn, unreadableLines)

main :: IO ()
main = do
    assertEqual "evicted status" (Just evictedStatus) (parseColdStatus evicted)
    assertEqual "onloading status lists only files that are not local" (Just onloadingStatus) (parseColdStatus onloadingJson)
    assertEqual "a fetched file of a still-evicted dataset is no longer listed" (Just partlyFetchedStatus) (parseColdStatus partlyFetched)
    assertEqual "a file fetch counts as onloading before the dataset phase says so" (Just fetchingStatus) (parseColdStatus fetchingJson)
    assertEqual "an output the cold store does not track is not cold" (Just (ColdStatus False False [])) (parseColdStatus untracked)
    assertEqual "evicted cold files" (Just (Set.singleton "SRR1.fastq.gz")) (parseColdFiles evicted)
    assertEqual "onloading cold files skip local files" (Just (Set.singleton "SRR1.fastq.gz")) (parseColdFiles onloadingJson)
    assertEqual "failed status has no dataset" Nothing (parseColdStatus "{\"ok\":false,\"error\":\"boom\"}")
    assertEqual "failed status leaves the cold files unknown" Nothing (parseColdFiles "{\"ok\":false,\"error\":\"boom\"}")
    assertEqual "malformed status" Nothing (parseColdStatus "not json")
    assertEqual "unknown path has no files" (Just Set.empty) (parseColdFiles "{\"ok\":true,\"status\":{\"datasets\":[]}}")
    assertEqual
        "offloaded files named in a build log, by step-relative or store path"
        [ ("step 428", "notes.txt")
        , ("step 428", "plain.txt")
        , ("/nix/store/bbbb-data", "nested dir/table.csv")
        , ("/nix/store/cccc-single", "cccc-single")
        , ("/nix/store/dddd-data", "a b.csv")
        , ("/nix/store/eeee-data", "a b.csv")
        , ("/nix/store/eeee-data", "b.csv")
        ]
        (offloadedFilesNamed (unreadableLines buildLog) coldByLabel)
    assertEqual "steps named by unreadable lines" [7, 428, 429] (stepIdsNamedIn (unreadableLines buildLog))
    assertEqual "prefetch response" (Right (PrefetchResponse True ["SRR1.fastq.gz"] [] [])) (parsePrefetchResponse prefetch)
    assertEqual "prefetch error" (Left "no token") (parsePrefetchResponse "{\"ok\":false,\"error\":\"no token\"}")

buildLog :: T.Text
buildLog =
    T.unlines
        [ "cat: 428/notes.txt: Input/output error"
        , "OSError: [Errno 5] Input/output error: '/nix/store/bbbb-data/nested dir/table.csv'"
        , "sh: can't open /nix/store/cccc-single: I/O error"
        , "cat: 12/summary.txt: No such file or directory"
        , "cat: ./7/my-other.txt: Input/output error"
        , "cat: 429/shared.txt: Input/output error"
        , "plain.txt: Input/output error"
        , "error: /nix/store/dddd-data/a b.csv is offloaded to cold storage; reads do not copy it back, so copy it back explicitly with prefetch or onload first"
        , "cat: '/nix/store/eeee-data/a b.csv': Input/output error"
        , "cat: /nix/store/eeee-data/b.csv: Input/output error"
        ]

coldByLabel :: [(T.Text, Set.Set T.Text)]
coldByLabel =
    [ ("step 428", Set.fromList ["notes.txt", "summary.txt", "other.txt", "shared.txt", "plain.txt"])
    , ("/nix/store/bbbb-data", Set.fromList ["nested dir/table.csv"])
    , ("/nix/store/cccc-single", Set.fromList ["cccc-single"])
    , ("/nix/store/dddd-data", Set.fromList ["a b.csv", "b.csv"])
    , ("/nix/store/eeee-data", Set.fromList ["a b.csv", "b.csv"])
    ]

evictedStatus :: ColdStatus
evictedStatus = ColdStatus True False [ColdFile "SRR1.fastq.gz" 0 0]

onloadingStatus :: ColdStatus
onloadingStatus = ColdStatus True True [ColdFile "SRR1.fastq.gz" 123 456]

partlyFetchedStatus :: ColdStatus
partlyFetchedStatus = ColdStatus True False [ColdFile "b.png" 0 0]

fetchingStatus :: ColdStatus
fetchingStatus = ColdStatus True True [ColdFile "b.png" 0 0, ColdFile "a.csv" 5 10]

evicted :: BS.ByteString
evicted =
    BS.pack $
        concat
            [ "{\"ok\":true,\"status\":{\"datasets\":[{\"store_path\":\"/nix/store/9hw-store-ref\","
            , "\"phase\":\"evicted\",\"local\":false,\"evicted\":true,"
            , "\"files\":[{\"path\":\"SRR1.fastq.gz\",\"size\":48204789210,\"local\":false,\"phase\":\"verified\",\"onload\":null}]}]}}"
            ]

onloadingJson :: BS.ByteString
onloadingJson =
    BS.pack $
        concat
            [ "{\"ok\":true,\"status\":{\"datasets\":[{\"store_path\":\"/nix/store/9hw-store-ref\","
            , "\"phase\":\"onloading\",\"local\":false,\"evicted\":false,"
            , "\"files\":[{\"path\":\"SRR1.fastq.gz\",\"size\":48204789210,\"local\":false,\"phase\":\"verified\","
            , "\"onload\":{\"bytes_done\":123,\"bytes_total\":456}},"
            , "{\"path\":\"SRR2.fastq.gz\",\"size\":10,\"local\":true,\"phase\":\"verified\",\"onload\":null}]}]}}"
            ]

partlyFetched :: BS.ByteString
partlyFetched =
    BS.pack $
        concat
            [ "{\"ok\":true,\"status\":{\"datasets\":[{\"store_path\":\"/nix/store/6id-alignment-histogram-1\","
            , "\"phase\":\"evicted\",\"local\":false,\"evicted\":true,\"files\":["
            , "{\"path\":\"a.csv\",\"local\":true,\"phase\":\"verified\",\"onload\":null},"
            , "{\"path\":\"b.png\",\"local\":false,\"phase\":\"verified\",\"onload\":null}]}]}}"
            ]

fetchingJson :: BS.ByteString
fetchingJson =
    BS.pack $
        concat
            [ "{\"ok\":true,\"status\":{\"datasets\":[{\"store_path\":\"/nix/store/6id-alignment-histogram-1\","
            , "\"phase\":\"evicted\",\"local\":false,\"evicted\":true,\"files\":["
            , "{\"path\":\"b.png\",\"local\":false,\"phase\":\"verified\",\"onload\":null},"
            , "{\"path\":\"a.csv\",\"local\":false,\"phase\":\"verified\",\"onload\":{\"bytes_done\":5,\"bytes_total\":10}}]}]}}"
            ]

untracked :: BS.ByteString
untracked = "{\"ok\":true,\"status\":{\"datasets\":[{\"store_path\":\"/nix/store/abc-built\",\"phase\":\"missing\",\"local\":false,\"files\":[]}]}}"

prefetch :: BS.ByteString
prefetch = "{\"ok\":true,\"started\":[\"SRR1.fastq.gz\"],\"skipped\":[],\"unknown\":[]}"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
