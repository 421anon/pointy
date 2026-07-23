{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Handlers.Download (extractDownloadHash, extractDownloadedAt, injectDownloaded)

main :: IO ()
main = do
    let request =
            object
                [ "args"
                    .= object
                        [ "url" .= ("https://example.com/data.csv" :: Text)
                        , "downloaded"
                            .= object
                                [ "hash" .= ("sha256-forged" :: Text)
                                , "downloadedAt" .= ("forged" :: Text)
                                , "extra" .= True
                                ]
                        ]
                ]
        timestamp = "2026-07-23T15:16:17Z"
        downloaded = injectDownloaded request "sha256-trusted" (Just timestamp)
        legacy = injectDownloaded request "sha256-trusted" Nothing

    assertEqual "trusted hash" (Just "sha256-trusted") (extractDownloadHash downloaded)
    assertEqual "trusted timestamp" (Just timestamp) (extractDownloadedAt downloaded)
    assertEqual "legacy timestamp omitted" Nothing (extractDownloadedAt legacy)
    assertEqual "client fields replaced" Nothing (downloadedField "extra" downloaded)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual

downloadedField :: Text -> Value -> Maybe Value
downloadedField key value = do
    Object root <- Just value
    Object args <- KM.lookup "args" root
    Object downloaded <- KM.lookup "downloaded" args
    KM.lookup (AK.fromText key) downloaded
