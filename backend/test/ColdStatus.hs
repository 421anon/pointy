{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Handlers.Cold (ColdFile (..), ColdStatus (..), PrefetchResponse (..), parseColdFiles, parseColdStatus, parsePrefetchResponse)

main :: IO ()
main = do
    assertEqual "evicted status" (Just evictedStatus) (parseColdStatus evicted)
    assertEqual "onloading status lists only files that are not local" (Just onloadingStatus) (parseColdStatus onloadingJson)
    assertEqual "a fetched file of a still-evicted dataset is no longer listed" (Just partlyFetchedStatus) (parseColdStatus partlyFetched)
    assertEqual "a file fetch counts as onloading before the dataset phase says so" (Just fetchingStatus) (parseColdStatus fetchingJson)
    assertEqual "evicted cold files" (Set.singleton "SRR1.fastq.gz") (parseColdFiles evicted)
    assertEqual "onloading cold files skip local files" (Set.singleton "SRR1.fastq.gz") (parseColdFiles onloadingJson)
    assertEqual "failed status has no dataset" Nothing (parseColdStatus "{\"ok\":false,\"error\":\"boom\"}")
    assertEqual "malformed status" Nothing (parseColdStatus "not json")
    assertEqual "unknown path has no files" Set.empty (parseColdFiles "{\"ok\":true,\"status\":{\"datasets\":[]}}")
    assertEqual "prefetch response" (Right (PrefetchResponse True ["SRR1.fastq.gz"] [] [])) (parsePrefetchResponse prefetch)
    assertEqual "prefetch error" (Left "no token") (parsePrefetchResponse "{\"ok\":false,\"error\":\"no token\"}")

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

prefetch :: BS.ByteString
prefetch = "{\"ok\":true,\"started\":[\"SRR1.fastq.gz\"],\"skipped\":[],\"unknown\":[]}"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
