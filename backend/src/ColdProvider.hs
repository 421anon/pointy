{-# LANGUAGE OverloadedStrings #-}

module ColdProvider (
    ColdStatus (..),
    ColdFile (..),
    ColdFileState (..),
    OffloadProgress (..),
    parseColdStatus,
    parseOffloadProgress,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

data ColdStatus = ColdStatus
    { coldStatusFiles :: [ColdFile]
    , coldStatusCapacityBytes :: Maybe Integer
    , coldStatusAvailableBytes :: Maybe Integer
    }
    deriving (Show, Eq)

data ColdFile = ColdFile
    { coldFilePath :: Text
    , coldFileSize :: Integer
    , coldFileState :: ColdFileState
    }
    deriving (Show, Eq)

data ColdFileState
    = Offloaded
    | Offloading Integer Integer
    | Onloading Integer Integer
    | OnloadFailed Text
    deriving (Show, Eq)

data OffloadProgress = OffloadProgress
    { offloadDone :: Integer
    , offloadTotal :: Integer
    }
    deriving (Show, Eq)

instance FromJSON ColdStatus where
    parseJSON = withObject "cold storage status" $ \o ->
        ColdStatus <$> o .: "files" <*> o .:? "capacityBytes" <*> o .:? "availableBytes"

instance ToJSON ColdStatus where
    toJSON status =
        object $
            ["files" .= coldStatusFiles status]
                ++ maybe [] (\total -> ["capacityBytes" .= total]) (coldStatusCapacityBytes status)
                ++ maybe [] (\available -> ["availableBytes" .= available]) (coldStatusAvailableBytes status)

instance FromJSON ColdFile where
    parseJSON = withObject "cold storage file" $ \o -> do
        path <- o .: "path"
        size <- o .: "size"
        state <- o .: "state"
        ColdFile path size <$> case state :: Text of
            "offloaded" -> pure Offloaded
            "offloading" -> Offloading <$> o .: "done" <*> o .: "total"
            "onloading" -> Onloading <$> o .: "done" <*> o .: "total"
            "failed" -> OnloadFailed <$> o .: "error"
            other -> fail ("unknown cold storage file state " ++ T.unpack other)

instance ToJSON ColdFile where
    toJSON file =
        object $
            ["path" .= coldFilePath file, "size" .= coldFileSize file] ++ case coldFileState file of
                Offloaded -> ["state" .= ("offloaded" :: Text)]
                Offloading done total -> ["state" .= ("offloading" :: Text), "done" .= done, "total" .= total]
                Onloading done total -> ["state" .= ("onloading" :: Text), "done" .= done, "total" .= total]
                OnloadFailed message -> ["state" .= ("failed" :: Text), "error" .= message]

instance FromJSON OffloadProgress where
    parseJSON = withObject "offload progress" $ \o -> OffloadProgress <$> o .: "done" <*> o .: "total"

parseColdStatus :: BS.ByteString -> Maybe ColdStatus
parseColdStatus = decodeStrict

parseOffloadProgress :: BS.ByteString -> Maybe OffloadProgress
parseOffloadProgress = decodeStrict
