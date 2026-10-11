{-# LANGUAGE OverloadedStrings #-}

module Search.Http (
    embedRequest,
    judgeRequest,
) where

import Config (EmbeddingConfig (..), JudgeConfig (..), ProviderConfig (..))
import Control.Exception (try)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Network.HTTP.Client (HttpException, Manager, Request (..), RequestBody (..), httpLbs, parseRequest, requestHeaders, responseBody, responseStatus, responseTimeoutMicro)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAuthorization, statusCode)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE sharedManager #-}
sharedManager :: Manager
sharedManager = unsafePerformIO newTlsManager

embedRequest :: EmbeddingConfig -> ProviderConfig -> [Text] -> IO (Either String [VU.Vector Float])
embedRequest embedding provider texts = do
    key <- lookupEnv (T.unpack (providerApiKeyEnv provider))
    case key of
        Nothing -> pure (Left ("The embedding provider key " <> T.unpack (providerApiKeyEnv provider) <> " is not set."))
        Just apiKey -> do
            let body =
                    object $
                        [ "model" .= embeddingModel embedding
                        , "input" .= texts
                        ]
                            ++ maybe [] (\dimensions -> ["dimensions" .= dimensions]) (embeddingDimensions embedding)
            response <- postJson provider apiKey "/embeddings" body
            pure (response >>= parseEmbeddings)

judgeRequest :: JudgeConfig -> ProviderConfig -> Text -> [(Int, Text)] -> IO (Either String (Map Int Double))
judgeRequest judge provider query candidates = do
    key <- lookupEnv (T.unpack (providerApiKeyEnv provider))
    case key of
        Nothing -> pure (Left ("The judge provider key " <> T.unpack (providerApiKeyEnv provider) <> " is not set."))
        Just apiKey -> do
            let state =
                    object
                        [ "search" .= query
                        , "steps" .= object [questionId stepId .= document | (stepId, document) <- candidates]
                        ]
                questions =
                    object
                        [ questionId stepId .= object ["type" .= String "noul", "instructions" .= instructionsFor stepId]
                        | (stepId, _) <- candidates
                        ]
                body = object ["model" .= judgeModel judge, "state" .= state, "questions" .= questions]
            response <- postJson provider apiKey "/systemone" body
            pure (response >>= parseJudge (map fst candidates))
  where
    instructionsFor stepId =
        T.replace "{id}" (T.pack (show stepId)) (T.replace "{query}" query (judgeInstructions judge))

questionId :: Int -> Key.Key
questionId stepId = Key.fromText ("s" <> T.pack (show stepId))

postJson :: ProviderConfig -> String -> String -> Value -> IO (Either String Value)
postJson provider apiKey endpoint body = do
    attempt <- try $ do
        request <- parseRequest (T.unpack (T.dropWhileEnd (== '/') (providerBaseUrl provider)) <> endpoint)
        let withHeaders =
                request
                    { method = "POST"
                    , responseTimeout = responseTimeoutMicro (max 1 (providerTimeoutSeconds provider) * 1000000)
                    , requestHeaders =
                        ("Content-Type", "application/json")
                            : (hAuthorization, BC.pack ("Bearer " <> apiKey))
                            : requestHeaders request
                    , requestBody = RequestBodyLBS (encode body)
                    }
        httpLbs withHeaders sharedManager
    pure $ case attempt of
        Left exception -> Left (show (exception :: HttpException))
        Right response
            | statusCode (responseStatus response) >= 400 ->
                Left ("Provider returned " <> show (statusCode (responseStatus response)) <> ": " <> take 500 (BC.unpack (BL.toStrict (responseBody response))))
            | otherwise ->
                either (Left . ("Could not decode the provider response: " <>)) Right (eitherDecode (responseBody response))

parseEmbeddings :: Value -> Either String [VU.Vector Float]
parseEmbeddings (Object root) = case KeyMap.lookup "data" root of
    Just (Array entries) -> map snd . sortOn fst <$> traverse parseEntry (zip [0 ..] (V.toList entries))
    _ -> Left "The embedding response has no data array."
  where
    parseEntry (position, Object entry) = do
        values <- case KeyMap.lookup "embedding" entry of
            Just (Array items) -> traverse scalar (V.toList items)
            _ -> Left "An embedding entry has no embedding array."
        let index = case KeyMap.lookup "index" entry of
                Just (Number value) -> fromMaybe position (toBoundedInteger value)
                _ -> position
        pure (index :: Int, normalize (VU.fromList values))
    parseEntry _ = Left "An embedding entry is not an object."
    scalar (Number value) = Right (toRealFloat value)
    scalar _ = Left "An embedding value is not a number."
parseEmbeddings _ = Left "The embedding response is not an object."

normalize :: VU.Vector Float -> VU.Vector Float
normalize vector
    | norm == 0 = vector
    | otherwise = VU.map (/ norm) vector
  where
    norm = sqrt (VU.sum (VU.map (\value -> value * value) vector))

parseJudge :: [Int] -> Value -> Either String (Map Int Double)
parseJudge stepIds (Object root) = case KeyMap.lookup "answers" root of
    Just (Object answers) ->
        Right $
            Map.fromList
                [ (stepId, toRealFloat probability)
                | stepId <- stepIds
                , Just (Object answer) <- [KeyMap.lookup (questionId stepId) answers]
                , Just (Number probability) <- [KeyMap.lookup "noul" answer]
                ]
    _ -> Left "The judge response has no answers object."
parseJudge _ _ = Left "The judge response is not an object."
