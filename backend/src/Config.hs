{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (..),
    UserRepoConfig (..),
    SlurmConfig (..),
    AgentConfig (..),
    ProviderConfig (..),
    ChatModelConfig (..),
    PiModelConfig (..),
    PiCompatConfig (..),
    PiCostConfig (..),
    EmbeddingConfig (..),
    JudgeConfig (..),
    SearchConfig (..),
    ChatProtocol (..),
    EmbeddingProtocol (..),
    JudgeProtocol (..),
    defaultAgentConfig,
    chatQualifier,
    chatProtocolName,
    loadConfig,
    decodeConfig,
    decodeConfigJson,
    configJson,
    configFields,
    renderConfig,
    warnMissingProviderKeys,
    defaultConfigPath,
    resolveConfigPath,
) where

import Autodocodec (HasCodec (..), JSONCodec, ObjectCodec, boolCodec, dimapCodec, listCodec, object, optionalFieldWith, optionalFieldWith', optionalFieldWithOmittedDefaultWith, parseJSONVia, requiredFieldWith, scientificCodec, stringCodec, stringConstCodec, textCodec, toJSONVia, (.=))
import ConfigSchema (ConfigField, codecFields, decodeToml, multilineTextCodec, providerNameCodec, renderToml)
import Control.Monad (forM_, when)
import Data.Aeson (Value)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (lookupEnv)

data UserRepoConfig = UserRepoConfig
    { userRepoUrl :: Text
    , userRepoKeyfile :: FilePath
    , userRepoBranch :: Text
    }
    deriving (Show)

data SlurmConfig = SlurmConfig
    { slurmPartition :: Text
    , slurmAccount :: Maybe Text
    , slurmTimeLimit :: Maybe Text
    , slurmExtra :: [Text]
    , slurmEnforcement :: Text
    }
    deriving (Show, Eq)

data AgentConfig = AgentConfig
    { agentSboxCommand :: FilePath
    , agentSboxArgs :: [Text]
    , agentRunnerCommand :: FilePath
    , agentRunnerArgs :: [Text]
    , agentTimeoutSeconds :: Int
    , agentOutputLimitBytes :: Int
    , agentSessionRetentionDays :: Int
    , agentBootstrapPrompt :: Text
    , agentTitlePrompt :: Text
    }
    deriving (Show, Eq)

data ProviderConfig = ProviderConfig
    { providerName :: Text
    , providerBaseUrl :: Text
    , providerApiKeyEnv :: Text
    , providerTimeoutSeconds :: Int
    }
    deriving (Show, Eq)

data ChatProtocol
    = ChatOpenAiCompletions
    | ChatOpenAiResponses
    | ChatAnthropicMessages
    | ChatGoogleGenerativeAi
    deriving (Show, Eq)

chatProtocolName :: ChatProtocol -> Text
chatProtocolName protocol = case protocol of
    ChatOpenAiCompletions -> "openai-completions"
    ChatOpenAiResponses -> "openai-responses"
    ChatAnthropicMessages -> "anthropic-messages"
    ChatGoogleGenerativeAi -> "google-generative-ai"

data EmbeddingProtocol = EmbeddingsOpenAi
    deriving (Show, Eq)

data JudgeProtocol = JudgeSystemOne
    deriving (Show, Eq)

data ChatModelConfig = ChatModelConfig
    { chatProvider :: Text
    , chatProtocol :: ChatProtocol
    , chatModel :: Text
    , chatThinking :: Maybe Text
    , chatPi :: PiModelConfig
    }
    deriving (Show, Eq)

data PiModelConfig = PiModelConfig
    { piName :: Maybe Text
    , piContextWindow :: Maybe Int
    , piMaxTokens :: Maybe Int
    , piInput :: [Text]
    , piReasoning :: Maybe Bool
    , piThinkingLevelMap :: Map Text Text
    , piDisabledThinkingLevels :: [Text]
    , piCost :: Maybe PiCostConfig
    , piCompat :: PiCompatConfig
    }
    deriving (Show, Eq)

data PiCostConfig = PiCostConfig
    { piCostInput :: Double
    , piCostOutput :: Double
    , piCostCacheRead :: Double
    , piCostCacheWrite :: Double
    }
    deriving (Show, Eq)

data PiCompatConfig = PiCompatConfig
    { piSupportsStore :: Maybe Bool
    , piSupportsDeveloperRole :: Maybe Bool
    , piSupportsReasoningEffort :: Maybe Bool
    , piSupportsUsageInStreaming :: Maybe Bool
    , piSupportsFinishReason :: Maybe Bool
    , piMaxTokensField :: Maybe Text
    , piRequiresToolResultName :: Maybe Bool
    , piRequiresAssistantAfterToolResult :: Maybe Bool
    , piRequiresThinkingAsText :: Maybe Bool
    , piRequiresReasoningContentOnAssistantMessages :: Maybe Bool
    , piThinkingFormat :: Maybe Text
    }
    deriving (Show, Eq)

emptyPiCompat :: PiCompatConfig
emptyPiCompat =
    PiCompatConfig
        { piSupportsStore = Nothing
        , piSupportsDeveloperRole = Nothing
        , piSupportsReasoningEffort = Nothing
        , piSupportsUsageInStreaming = Nothing
        , piSupportsFinishReason = Nothing
        , piMaxTokensField = Nothing
        , piRequiresToolResultName = Nothing
        , piRequiresAssistantAfterToolResult = Nothing
        , piRequiresThinkingAsText = Nothing
        , piRequiresReasoningContentOnAssistantMessages = Nothing
        , piThinkingFormat = Nothing
        }

emptyPiModel :: PiModelConfig
emptyPiModel =
    PiModelConfig
        { piName = Nothing
        , piContextWindow = Nothing
        , piMaxTokens = Nothing
        , piInput = []
        , piReasoning = Nothing
        , piThinkingLevelMap = Map.empty
        , piDisabledThinkingLevels = []
        , piCost = Nothing
        , piCompat = emptyPiCompat
        }

data EmbeddingConfig = EmbeddingConfig
    { embeddingProvider :: Text
    , embeddingProtocol :: EmbeddingProtocol
    , embeddingModel :: Text
    , embeddingDimensions :: Maybe Int
    , embeddingQueryTemplate :: Text
    , embeddingDocumentTemplate :: Text
    , embeddingBatchSize :: Int
    , embeddingMinSimilarity :: Double
    }
    deriving (Show, Eq)

emptyEmbeddingConfig :: EmbeddingConfig
emptyEmbeddingConfig =
    EmbeddingConfig
        { embeddingProvider = ""
        , embeddingProtocol = EmbeddingsOpenAi
        , embeddingModel = ""
        , embeddingDimensions = Nothing
        , embeddingQueryTemplate = "{query}"
        , embeddingDocumentTemplate = "{document}"
        , embeddingBatchSize = 128
        , embeddingMinSimilarity = 0.4
        }

data JudgeConfig = JudgeConfig
    { judgeProvider :: Text
    , judgeProtocol :: JudgeProtocol
    , judgeModel :: Text
    , judgeInstructions :: Text
    , judgeMinRelevance :: Double
    }
    deriving (Show)

data SearchConfig = SearchConfig
    { searchLimit :: Int
    , searchJudgeCandidates :: Int
    , searchEmbedding :: EmbeddingConfig
    , searchJudge :: Maybe JudgeConfig
    }
    deriving (Show)

data Config where
    Config ::
        { configUserRepo :: UserRepoConfig
        , configSlurm :: SlurmConfig
        , configAgent :: AgentConfig
        , configProviders :: Map Text ProviderConfig
        , configChat :: Maybe ChatModelConfig
        , configSearch :: Maybe SearchConfig
        } ->
        Config
    deriving (Show)

chatQualifier :: ChatModelConfig -> Text
chatQualifier chat = case chatThinking chat of
    Nothing -> chatProvider chat <> "/" <> chatModel chat
    Just level -> chatProvider chat <> "/" <> chatModel chat <> ":" <> level

userRepoCodec :: JSONCodec UserRepoConfig
userRepoCodec =
    object "UserRepo" $
        UserRepoConfig
            <$> requiredFieldWith "url" textCodec "SSH clone URL of the repository holding templates and data." .= userRepoUrl
            <*> requiredFieldWith "keyfile" stringCodec "Private deploy key the backend reaches the repository with. Generated on startup when missing." .= userRepoKeyfile
            <*> requiredFieldWith "branch" textCodec "Branch the instance works on." .= userRepoBranch

defaultSlurmConfig :: SlurmConfig
defaultSlurmConfig = SlurmConfig "" Nothing Nothing [] "scheduler"

slurmCodec :: JSONCodec SlurmConfig
slurmCodec =
    object "Slurm" $
        SlurmConfig
            <$> defaulted "partition" textCodec slurmPartition "Partition builds are submitted to. Empty uses Slurm's default."
            <*> optionalFieldWith "account" textCodec "Account builds are charged to. Unset uses Slurm's default." .= slurmAccount
            <*> optionalFieldWith "time-limit" textCodec "Time limit passed to sbatch --time. Unset uses Slurm's default." .= slurmTimeLimit
            <*> defaulted "extra" textListCodec slurmExtra "Extra arguments passed to sbatch."
            <*> defaulted "enforcement" (textEnumCodec ("scheduler" :| ["metadata-only"])) slurmEnforcement "scheduler passes a step's CPU and memory requirements to Slurm; metadata-only records them without enforcing them."
  where
    defaulted :: (Eq field) => Text -> JSONCodec field -> (SlurmConfig -> field) -> Text -> ObjectCodec SlurmConfig field
    defaulted = defaultedFrom defaultSlurmConfig

agentCodec :: JSONCodec AgentConfig
agentCodec =
    object "Agent" $
        AgentConfig
            <$> defaulted "sbox-command" stringCodec agentSboxCommand "Sandbox command each agent turn runs in, started from {home}."
            <*> defaulted "sbox-args" textListCodec agentSboxArgs "Arguments to sbox-command. {worktree} expands to the session worktree, {home} to the session's home directory, {sessionRoot} to the worktree's parent and {sessionId} to the session id."
            <*> defaulted "runner-command" stringCodec agentRunnerCommand "Agent runner started inside the sandbox."
            <*> defaulted "runner-args" textListCodec agentRunnerArgs "Extra arguments to runner-command. The chat model is passed separately."
            <*> defaulted "timeout-seconds" intCodec agentTimeoutSeconds "Longest an agent turn may run, in seconds."
            <*> defaulted "output-limit-bytes" intCodec agentOutputLimitBytes "Most output kept from an agent turn, in bytes."
            <*> defaulted "session-retention-days" intCodec agentSessionRetentionDays "Days agent sessions are kept."
            <*> defaulted "bootstrap-prompt" multilineTextCodec agentBootstrapPrompt "Prompt run once against a fresh checkout so new chats start with project context. Empty disables it."
            <*> defaulted "title-prompt" multilineTextCodec agentTitlePrompt "Prompt that names a chat after its first message."
  where
    defaulted :: (Eq field) => Text -> JSONCodec field -> (AgentConfig -> field) -> Text -> ObjectCodec AgentConfig field
    defaulted = defaultedFrom defaultAgentConfig

providersCodec :: JSONCodec (Map Text ProviderConfig)
providersCodec = dimapCodec (Map.fromList . map (\provider -> (providerName provider, provider))) Map.elems (listCodec providerCodec)

providerCodec :: JSONCodec ProviderConfig
providerCodec =
    object "Provider" $
        ProviderConfig
            <$> requiredFieldWith "name" textCodec "Name models refer to in their provider field." .= providerName
            <*> requiredFieldWith "base-url" textCodec "Base URL of the provider's API." .= providerBaseUrl
            <*> requiredFieldWith "api-key-env" textCodec "Environment variable holding the API key, so keys never live in this file." .= providerApiKeyEnv
            <*> optionalFieldWithOmittedDefaultWith "timeout-seconds" intCodec 10 "Request timeout in seconds." .= providerTimeoutSeconds

chatModelCodec :: JSONCodec ChatModelConfig
chatModelCodec =
    object "ChatModel" $
        ChatModelConfig
            <$> requiredFieldWith "provider" providerNameCodec "Provider the model runs on." .= chatProvider
            <*> requiredFieldWith "protocol" chatProtocolCodec "API the provider serves the model with." .= chatProtocol
            <*> requiredFieldWith "model" textCodec "Model id at the provider." .= chatModel
            <*> optionalFieldWith "thinking" textCodec "Thinking level the agent uses, e.g. high." .= chatThinking
            <*> optionalFieldWithOmittedDefaultWith "pi" piModelCodec emptyPiModel "Model details written into Pi's models.json." .= chatPi

chatProtocolCodec :: JSONCodec ChatProtocol
chatProtocolCodec =
    stringConstCodec ((\protocol -> (protocol, chatProtocolName protocol)) <$> ChatOpenAiCompletions :| [ChatOpenAiResponses, ChatAnthropicMessages, ChatGoogleGenerativeAi])

piModelCodec :: JSONCodec PiModelConfig
piModelCodec =
    object "PiModel" $
        PiModelConfig
            <$> optionalFieldWith "name" textCodec "Name Pi shows for the model." .= piName
            <*> optionalFieldWith "contextWindow" intCodec "Context window in tokens." .= piContextWindow
            <*> optionalFieldWith "maxTokens" intCodec "Most tokens a response may use." .= piMaxTokens
            <*> defaulted "input" textListCodec piInput "Input types the model accepts, e.g. text and image."
            <*> optionalFieldWith "reasoning" boolCodec "Whether the model reasons before answering." .= piReasoning
            <*> defaulted "thinkingLevelMap" codec piThinkingLevelMap "Pi thinking levels mapped to the model's own levels."
            <*> defaulted "disabledThinkingLevels" textListCodec piDisabledThinkingLevels "Pi thinking levels the model does not support; they are hidden and skipped when cycling."
            <*> optionalFieldWith "cost" piCostCodec "Prices per million tokens." .= piCost
            <*> defaulted "compat" piCompatCodec piCompat "Compatibility flags for the provider's API."
  where
    defaulted :: (Eq field) => Text -> JSONCodec field -> (PiModelConfig -> field) -> Text -> ObjectCodec PiModelConfig field
    defaulted = defaultedFrom emptyPiModel

piCostCodec :: JSONCodec PiCostConfig
piCostCodec =
    object "PiCost" $
        PiCostConfig
            <$> requiredFieldWith "input" doubleCodec "Input tokens." .= piCostInput
            <*> requiredFieldWith "output" doubleCodec "Output tokens." .= piCostOutput
            <*> requiredFieldWith "cacheRead" doubleCodec "Tokens read from the cache." .= piCostCacheRead
            <*> requiredFieldWith "cacheWrite" doubleCodec "Tokens written to the cache." .= piCostCacheWrite

piCompatCodec :: JSONCodec PiCompatConfig
piCompatCodec =
    object "PiCompat" $
        PiCompatConfig
            <$> optionalFieldWith' "supportsStore" boolCodec .= piSupportsStore
            <*> optionalFieldWith' "supportsDeveloperRole" boolCodec .= piSupportsDeveloperRole
            <*> optionalFieldWith' "supportsReasoningEffort" boolCodec .= piSupportsReasoningEffort
            <*> optionalFieldWith' "supportsUsageInStreaming" boolCodec .= piSupportsUsageInStreaming
            <*> optionalFieldWith' "supportsFinishReason" boolCodec .= piSupportsFinishReason
            <*> optionalFieldWith' "maxTokensField" textCodec .= piMaxTokensField
            <*> optionalFieldWith' "requiresToolResultName" boolCodec .= piRequiresToolResultName
            <*> optionalFieldWith' "requiresAssistantAfterToolResult" boolCodec .= piRequiresAssistantAfterToolResult
            <*> optionalFieldWith' "requiresThinkingAsText" boolCodec .= piRequiresThinkingAsText
            <*> optionalFieldWith' "requiresReasoningContentOnAssistantMessages" boolCodec .= piRequiresReasoningContentOnAssistantMessages
            <*> optionalFieldWith' "thinkingFormat" textCodec .= piThinkingFormat

embeddingCodec :: JSONCodec EmbeddingConfig
embeddingCodec =
    object "Embedding" $
        EmbeddingConfig
            <$> requiredFieldWith "provider" providerNameCodec "Provider the embedding model runs on. None keeps step search lexical." .= embeddingProvider
            <*> requiredFieldWith "protocol" (stringConstCodec ((EmbeddingsOpenAi, "openai-embeddings") :| [])) "API the provider serves the model with." .= embeddingProtocol
            <*> requiredFieldWith "model" textCodec "Model id at the provider." .= embeddingModel
            <*> optionalFieldWith "dimensions" intCodec "Embedding size to request." .= embeddingDimensions
            <*> defaulted "query-template" multilineTextCodec embeddingQueryTemplate "Text embedded for a search, with {query} standing for the search."
            <*> defaulted "document-template" multilineTextCodec embeddingDocumentTemplate "Text embedded for a step, with {document} standing for the step."
            <*> defaulted "batch-size" intCodec embeddingBatchSize "Steps embedded per request."
            <*> defaulted "min-similarity" doubleCodec embeddingMinSimilarity "Lowest similarity a related step may have."
  where
    defaulted :: (Eq field) => Text -> JSONCodec field -> (EmbeddingConfig -> field) -> Text -> ObjectCodec EmbeddingConfig field
    defaulted = defaultedFrom emptyEmbeddingConfig

judgeCodec :: JSONCodec JudgeConfig
judgeCodec =
    object "Judge" $
        JudgeConfig
            <$> requiredFieldWith "provider" providerNameCodec "Provider the judge model runs on." .= judgeProvider
            <*> requiredFieldWith "protocol" (stringConstCodec ((JudgeSystemOne, "systemone") :| [])) "API the provider serves the model with." .= judgeProtocol
            <*> requiredFieldWith "model" textCodec "Model id at the provider." .= judgeModel
            <*> optionalFieldWithOmittedDefaultWith "instructions" multilineTextCodec "Is step {id} one of the steps the person searching for \"{query}\" wants to find?" "Question the judge answers for each candidate, with {id} standing for the step and {query} for the search." .= judgeInstructions
            <*> optionalFieldWithOmittedDefaultWith "min-relevance" doubleCodec 0.5 "Lowest relevance a related step may have." .= judgeMinRelevance

searchCodec :: JSONCodec SearchConfig
searchCodec =
    object "Search" $
        SearchConfig
            <$> optionalFieldWithOmittedDefaultWith "limit" intCodec 10 "Most related steps shown." .= searchLimit
            <*> optionalFieldWithOmittedDefaultWith "judge-candidates" intCodec 24 "Related steps the judge reviews, at most 64." .= searchJudgeCandidates
            <*> optionalFieldWithOmittedDefaultWith "embedding" embeddingCodec emptyEmbeddingConfig "Embedding model that finds related steps." .= searchEmbedding
            <*> optionalFieldWith "judge" judgeCodec "Model that keeps only the related steps the search is after. Off shows them all." .= searchJudge

configCodec :: JSONCodec Config
configCodec =
    object "Config" $
        (\userRepo providers chat search slurm agent -> Config userRepo slurm agent providers chat search)
            <$> requiredFieldWith "user-repo" userRepoCodec "Repository holding templates and data." .= configUserRepo
            <*> optionalFieldWithOmittedDefaultWith "providers" providersCodec Map.empty "AI providers models run on." .= configProviders
            <*> optionalFieldWithOmittedDefaultWith "models" modelsCodec Nothing "Models the instance uses." .= configChat
            <*> optionalFieldWith "search" searchCodec "Embedding and judge models that add a Related section to step search. Off keeps search lexical." .= configSearch
            <*> optionalFieldWithOmittedDefaultWith "slurm" slurmCodec defaultSlurmConfig "How builds are submitted to Slurm." .= configSlurm
            <*> optionalFieldWithOmittedDefaultWith "agent" agentCodec defaultAgentConfig "How the agent panel runs agent turns." .= configAgent
  where
    modelsCodec = object "Models" (optionalFieldWith "chat" chatModelCodec "Chat model the agent panel runs on. Off leaves the agent without a model." .= id)

defaultedFrom :: (Eq field) => record -> Text -> JSONCodec field -> (record -> field) -> Text -> ObjectCodec record field
defaultedFrom defaults key fieldCodec field doc = optionalFieldWithOmittedDefaultWith key fieldCodec (field defaults) doc .= field

textListCodec :: JSONCodec [Text]
textListCodec = listCodec textCodec

textEnumCodec :: NonEmpty Text -> JSONCodec Text
textEnumCodec = stringConstCodec . fmap (\value -> (value, value))

intCodec :: JSONCodec Int
intCodec = codec

doubleCodec :: JSONCodec Double
doubleCodec = dimapCodec Scientific.toRealFloat Scientific.fromFloatDigits scientificCodec

configFields :: [ConfigField]
configFields = codecFields configCodec

configJson :: Config -> Value
configJson = toJSONVia configCodec

renderConfig :: Config -> Text
renderConfig = renderToml configFields . configJson

decodeConfig :: Text -> Either String Config
decodeConfig contents = decodeToml contents >>= decodeConfigJson

decodeConfigJson :: Value -> Either String Config
decodeConfigJson = first (T.unpack . readable . T.pack) . parseEither (parseJSONVia configCodec)
  where
    readable message = maybe message located (T.stripPrefix "Error in $" message)
    located rest = case T.breakOn ":" rest of
        ("", detail) -> explained detail
        (path, detail) -> T.dropWhile (== '.') (T.replace "']" "" (T.replace "['" "." path)) <> ": " <> explained detail
    explained detail = case map (T.takeWhile (/= '"') . T.drop 10 . snd) (T.breakOnAll "Expected \"" detail) of
        [] -> T.strip (T.drop 1 detail)
        choices -> "expected one of " <> T.intercalate ", " choices

defaultAgentConfig :: AgentConfig
defaultAgentConfig =
    AgentConfig
        { agentSboxCommand = "sbox"
        , agentSboxArgs = ["--bind", "{home}", "{home}"]
        , agentRunnerCommand = "pi"
        , agentRunnerArgs = []
        , agentTimeoutSeconds = 1800
        , agentOutputLimitBytes = 1048576
        , agentSessionRetentionDays = 7
        , agentBootstrapPrompt = "Read AGENTS.md and skill://pointy-router. Do not modify any files. Reply READY when you understand the keyword skill map for non-technical user requests."
        , agentTitlePrompt = "Name the chat that opens with the request in this message. Reply with the title only: five words at most, no quotes, no punctuation, no explanation."
        }

defaultConfigPath :: FilePath
defaultConfigPath = "/home/backend/config.toml"

resolveConfigPath :: IO FilePath
resolveConfigPath = fromMaybe defaultConfigPath <$> lookupEnv "POINTY_CONFIG_PATH"

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- decodeUtf8With lenientDecode <$> BS.readFile path
    case decodeConfig contents of
        Left err -> error ("Failed to parse config: " ++ err)
        Right config -> do
            validateConfig config
            pure config

validateConfig :: Config -> IO ()
validateConfig config = do
    forM_ (providerReferences config) $ \(name, section) ->
        when (Map.notMember name (configProviders config)) $
            error $
                "Failed to parse config: " ++ section ++ " names provider \"" ++ T.unpack name ++ "\", which is not in [providers]"
    when (maybe False ((> 64) . searchJudgeCandidates) (configSearch config)) $
        error "Failed to parse config: [search] judge-candidates must be at most 64"

warnMissingProviderKeys :: Config -> IO ()
warnMissingProviderKeys config =
    forM_ (providerReferences config) $ \(name, section) ->
        forM_ (Map.lookup name (configProviders config)) $ \provider -> do
            key <- lookupEnv (T.unpack (providerApiKeyEnv provider))
            when (key == Nothing) $
                putStrLn $
                    "Warning: "
                        ++ section
                        ++ " provider \""
                        ++ T.unpack name
                        ++ "\" reads its key from "
                        ++ T.unpack (providerApiKeyEnv provider)
                        ++ ", which is not set"

providerReferences :: Config -> [(Text, String)]
providerReferences config =
    [ (chatProvider chat, "[models.chat]")
    | chat <- mapMaybe id [configChat config]
    ]
        ++ [ (embeddingProvider (searchEmbedding search), "[search.embedding]")
           | search <- mapMaybe id [configSearch config]
           , not (T.null (embeddingProvider (searchEmbedding search)))
           ]
        ++ [ (judgeProvider judge, "[search.judge]")
           | search <- mapMaybe id [configSearch config]
           , judge <- mapMaybe id [searchJudge search]
           ]
