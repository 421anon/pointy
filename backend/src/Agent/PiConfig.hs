{-# LANGUAGE OverloadedStrings #-}

module Agent.PiConfig (writePiConfig) where

import Agent.Sandbox (piAgentConfigDir)
import Config (ChatModelConfig (..), PiCompatConfig (..), PiCostConfig (..), PiModelConfig (..), ProviderConfig (..), chatProtocolName)
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)

writePiConfig :: Map Text ProviderConfig -> [ChatModelConfig] -> IO ()
writePiConfig providers chats = do
    dir <- piAgentConfigDir
    createDirectoryIfMissing True dir
    writeIfChanged (dir </> "models.json") (encode (object ["providers" .= object providerEntries]))
    writeIfChanged (dir </> "settings.json") (encode (maybe (object ["defaultProjectTrust" .= String "always"]) settingsJson (listToMaybe chats)))
  where
    providerEntries =
        [ Key.fromText name .= providerJson provider [chat | chat <- chats, chatProvider chat == name]
        | name <- nub (map chatProvider chats)
        , Just provider <- [Map.lookup name providers]
        ]

providerJson :: ProviderConfig -> [ChatModelConfig] -> Value
providerJson provider chats =
    object
        [ "baseUrl" .= providerBaseUrl provider
        , "apiKey" .= ("$" <> providerApiKeyEnv provider)
        , "models" .= map modelJson chats
        ]

modelJson :: ChatModelConfig -> Value
modelJson chat =
    object (("id" .= chatModel chat) : ("api" .= chatProtocolName (chatProtocol chat)) : piModelFields (chatPi chat))

piModelFields :: PiModelConfig -> [(Key.Key, Value)]
piModelFields model =
    catMaybes
        [ fmap (\name -> "name" .= String name) (piName model)
        , fmap (\window -> "contextWindow" .= Number (fromIntegral window)) (piContextWindow model)
        , fmap (\maxTokens -> "maxTokens" .= Number (fromIntegral maxTokens)) (piMaxTokens model)
        , if null (piInput model) then Nothing else Just ("input" .= map String (piInput model))
        , fmap (\reasoning -> "reasoning" .= Bool reasoning) (piReasoning model)
        , if Map.null levels then Nothing else Just ("thinkingLevelMap" .= levels)
        , if Map.null (piHeaders model) then Nothing else Just ("headers" .= piHeaders model)
        , fmap (\cost -> "cost" .= costJson cost) (piCost model)
        , case compatFields (piCompat model) of
            [] -> Nothing
            fields -> Just ("compat" .= object fields)
        ]
  where
    levels =
        Map.union
            (Map.fromList [(level, Null) | level <- piDisabledThinkingLevels model])
            (Map.map String (piThinkingLevelMap model))

costJson :: PiCostConfig -> Value
costJson cost =
    object
        [ "input" .= piCostInput cost
        , "output" .= piCostOutput cost
        , "cacheRead" .= piCostCacheRead cost
        , "cacheWrite" .= piCostCacheWrite cost
        ]

compatFields :: PiCompatConfig -> [(Key.Key, Value)]
compatFields compat =
    catMaybes
        [ fmap (\value -> "supportsStore" .= Bool value) (piSupportsStore compat)
        , fmap (\value -> "supportsDeveloperRole" .= Bool value) (piSupportsDeveloperRole compat)
        , fmap (\value -> "supportsReasoningEffort" .= Bool value) (piSupportsReasoningEffort compat)
        , fmap (\value -> "supportsUsageInStreaming" .= Bool value) (piSupportsUsageInStreaming compat)
        , fmap (\value -> "supportsFinishReason" .= Bool value) (piSupportsFinishReason compat)
        , fmap (\value -> "maxTokensField" .= String value) (piMaxTokensField compat)
        , fmap (\value -> "requiresToolResultName" .= Bool value) (piRequiresToolResultName compat)
        , fmap (\value -> "requiresAssistantAfterToolResult" .= Bool value) (piRequiresAssistantAfterToolResult compat)
        , fmap (\value -> "requiresThinkingAsText" .= Bool value) (piRequiresThinkingAsText compat)
        , fmap (\value -> "requiresReasoningContentOnAssistantMessages" .= Bool value) (piRequiresReasoningContentOnAssistantMessages compat)
        , fmap (\value -> "thinkingFormat" .= String value) (piThinkingFormat compat)
        ]

settingsJson :: ChatModelConfig -> Value
settingsJson chat =
    object $
        [ "defaultProvider" .= chatProvider chat
        , "defaultModel" .= chatModel chat
        , "defaultProjectTrust" .= String "always"
        ]
            ++ maybe [] (\level -> ["defaultThinkingLevel" .= level]) (chatThinking chat)

writeIfChanged :: FilePath -> BL.ByteString -> IO ()
writeIfChanged path bytes = do
    let contents = BL.toStrict bytes
    current <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    unless (either (const False) (== contents) current) $ do
        (staging, handle) <- openBinaryTempFile (takeDirectory path) (takeFileName path)
        BS.hPut handle contents
        hClose handle
        renameFile staging path
