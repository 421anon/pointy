{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Images (ImageUpload (..), PromptImage (..), decodeImageUploads, imagesByteLimit)
import Data.Aeson (eitherDecode)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Handlers.Agent (TurnRequest (..), postTurnHandler, steerTurnHandler)
import Servant.Server (ServerError (..), runHandler)

main :: IO ()
main = do
    assertEqual
        "requests without images decode to no images"
        (Right [])
        (turnRequestImages <$> eitherDecode "{\"sessionId\":\"s1\",\"prompt\":\"hi\"}")
    assertEqual
        "request images decode their data"
        (Right [ImageUpload pngData])
        (turnRequestImages <$> eitherDecode (LBS.fromStrict (TE.encodeUtf8 ("{\"sessionId\":\"s1\",\"prompt\":\"\",\"images\":[{\"data\":\"" <> pngData <> "\"}]}"))))

    assertEqual "invalid base64 is rejected" (Left "image_data_invalid") (decodeImageUploads [ImageUpload "not base64!"])
    assertEqual "data that is no supported image is rejected" (Left "image_data_invalid") (decodeImageUploads [ImageUpload (base64 "<svg xmlns=\"http://www.w3.org/2000/svg\"/>")])
    assertEqual "empty data is rejected" (Left "image_data_invalid") (decodeImageUploads [ImageUpload ""])
    let half = base64 (pngSignature <> BS.replicate (imagesByteLimit `div` 2) 0)
    assertEqual "images over the total cap are rejected" (Left "images_too_large") (decodeImageUploads [ImageUpload half, ImageUpload half])
    assertEqual
        "valid images decode to their bytes"
        (Right [("image/png", pngSignature <> "pixels"), ("image/jpeg", jpegBytes)])
        (map (\image -> (promptImageMimeType image, promptImageBytes image)) <$> decodeImageUploads [ImageUpload pngData, ImageUpload (base64 jpegBytes)])

    rejectedTurn <- runHandler (postTurnHandler (turnRequest [ImageUpload "%%%"]))
    assertEqual "turns with invalid images are refused" (Left (400, "image_data_invalid")) (failure rejectedTurn)
    rejectedSteer <- runHandler (steerTurnHandler (turnRequest [ImageUpload pngData]))
    assertEqual "steering with images is refused" (Left (400, "images_not_supported_when_steering")) (failure rejectedSteer)

pngSignature :: BS.ByteString
pngSignature = "\x89PNG\r\n\x1a\n"

pngData :: Text
pngData = base64 (pngSignature <> "pixels")

jpegBytes :: BS.ByteString
jpegBytes = "\xff\xd8\xff\xe0jfif"

base64 :: BS.ByteString -> Text
base64 = TE.decodeLatin1 . convertToBase Base64

turnRequest :: [ImageUpload] -> TurnRequest
turnRequest images =
    TurnRequest
        { turnRequestSessionId = "s1"
        , turnRequestPrompt = "look"
        , turnRequestCurrentProjectId = Nothing
        , turnRequestAutoApply = True
        , turnRequestClientId = Nothing
        , turnRequestImages = images
        }

failure :: Either ServerError a -> Either (Int, LBS.ByteString) ()
failure = either (\err -> Left (errHTTPCode err, errBody err)) (const (Right ()))

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
