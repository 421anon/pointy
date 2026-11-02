{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Handlers.Cold (ColdFile (..), ColdStatus (..), PrefetchResponse (..), parseColdFiles, parseColdStatus, parsePrefetchResponse)

main :: IO ()
main = do
    assertEqual "evicted status" (Just evictedStatus) (parseColdStatus evicted)
    assertEqual "onloading status" (Just onloadingStatus) (parseColdStatus onloadingJson)
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
onloadingStatus = ColdStatus True True [ColdFile "SRR1.fastq.gz" 123 456, ColdFile "SRR2.fastq.gz" 0 0]

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

prefetch :: BS.ByteString
prefetch = "{\"ok\":true,\"started\":[\"SRR1.fastq.gz\"],\"skipped\":[],\"unknown\":[]}"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
