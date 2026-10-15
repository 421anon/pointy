{-# LANGUAGE OverloadedStrings #-}

module Agent.Images (
    ImageUpload (..),
    PromptImage (..),
    imageMimeTypes,
    imagesByteLimit,
    decodeImageUploads,
    promptImageRecord,
    piImageContent,
) where

import Agent.Session (AgentImage (..))
import Control.Lens (filtered, folded, sumOf, to, (^?), _1, _Right)
import Control.Monad (when)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.=))
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

newtype ImageUpload = ImageUpload {uploadData :: Text}
    deriving (Show, Eq)

instance FromJSON ImageUpload where
    parseJSON = withObject "ImageUpload" $ fmap ImageUpload . (.: "data")

data PromptImage = PromptImage
    { promptImageMimeType :: Text
    , promptImageData :: Text
    , promptImageBytes :: BS.ByteString
    }
    deriving (Show, Eq)

imagesByteLimit :: Int
imagesByteLimit = 6 * 1024 * 1024

imageSignatures :: [(Text, BS.ByteString -> Bool)]
imageSignatures =
    [ ("image/png", BS.isPrefixOf "\x89PNG\r\n\x1a\n")
    , ("image/jpeg", BS.isPrefixOf "\xff\xd8\xff")
    , ("image/gif", \bytes -> any (`BS.isPrefixOf` bytes) ["GIF87a", "GIF89a"])
    , ("image/webp", \bytes -> BS.take 4 bytes == "RIFF" && BS.take 4 (BS.drop 8 bytes) == "WEBP")
    ]

imageMimeTypes :: [Text]
imageMimeTypes = map fst imageSignatures

decodeImageUploads :: [ImageUpload] -> Either String [PromptImage]
decodeImageUploads uploads = do
    images <- traverse (maybe (Left "image_data_invalid") Right . decodeUpload) uploads
    when (sumOf (folded . to (BS.length . promptImageBytes)) images > imagesByteLimit) $ Left "images_too_large"
    pure images
  where
    decodeUpload upload = do
        bytes <- convertFromBase Base64 (TE.encodeUtf8 (uploadData upload)) ^? _Right
        mimeType <- imageSignatures ^? folded . filtered (($ bytes) . snd) . _1
        pure (PromptImage mimeType (uploadData upload) bytes)

promptImageRecord :: PromptImage -> AgentImage
promptImageRecord = AgentImage . promptImageMimeType

piImageContent :: PromptImage -> Value
piImageContent image =
    object
        [ "type" .= ("image" :: Text)
        , "data" .= promptImageData image
        , "mimeType" .= promptImageMimeType image
        ]
