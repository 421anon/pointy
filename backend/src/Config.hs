{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    warnMissingProviderKeys,
    defaultConfigPath,
    resolveConfigPath,
) where

import Control.Monad (forM_, when)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml
import Toml.Codec.Types (Codec (..))
import Toml.Type.Key (KeysDiff (..), keysDiff, pattern (:||))
import Toml.Type.PrefixTree (PrefixMap, PrefixTree (..))
import qualified Toml.Type.PrefixTree as Prefix
import Toml.Type.TOML (TOML (..))

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
    deriving (Show)

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
    deriving (Show)

data ProviderConfig = ProviderConfig
    { providerName :: Text
    , providerBaseUrl :: Text
    , providerApiKeyEnv :: Text
    , providerTimeoutSeconds :: Int
    }
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

data PiCostConfig = PiCostConfig
    { piCostInput :: Double
    , piCostOutput :: Double
    , piCostCacheRead :: Double
    , piCostCacheWrite :: Double
    }
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

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

userRepoCodec :: TomlCodec UserRepoConfig
userRepoCodec =
    UserRepoConfig
        <$> Toml.text "url" .= userRepoUrl
        <*> Toml.string "keyfile" .= userRepoKeyfile
        <*> Toml.text "branch" .= userRepoBranch

defaultSlurmConfig :: SlurmConfig
defaultSlurmConfig = SlurmConfig "" Nothing Nothing [] "scheduler"

slurmCodec :: TomlCodec SlurmConfig
slurmCodec =
    SlurmConfig
        <$> Toml.dimap nonEmptyText (fromMaybe "") (Toml.dioptional (Toml.text "partition")) .= slurmPartition
        <*> Toml.dioptional (Toml.text "account") .= slurmAccount
        <*> Toml.dioptional (Toml.text "time-limit") .= slurmTimeLimit
        <*> Toml.dimap nonEmptyExtra (fromMaybe []) (Toml.dioptional (Toml.arrayOf Toml._Text "extra")) .= slurmExtra
        <*> Toml.dimap nonEmptyText (fromMaybe "scheduler") (Toml.dioptional (Toml.text "enforcement")) .= slurmEnforcement
  where
    nonEmptyText text
        | text == "" = Nothing
        | otherwise = Just text
    nonEmptyExtra extra = case extra of
        [] -> Nothing
        xs -> Just xs

agentCodec :: TomlCodec AgentConfig
agentCodec =
    mkAgentConfig
        <$> Toml.dioptional (Toml.string "sbox-command") .= (Just . agentSboxCommand)
        <*> Toml.dioptional (Toml.arrayOf Toml._Text "sbox-args") .= (Just . agentSboxArgs)
        <*> Toml.dioptional (Toml.string "runner-command") .= (Just . agentRunnerCommand)
        <*> Toml.dioptional (Toml.arrayOf Toml._Text "runner-args") .= (Just . agentRunnerArgs)
        <*> Toml.dioptional (Toml.int "timeout-seconds") .= (Just . agentTimeoutSeconds)
        <*> Toml.dioptional (Toml.int "output-limit-bytes") .= (Just . agentOutputLimitBytes)
        <*> Toml.dioptional (Toml.int "session-retention-days") .= (Just . agentSessionRetentionDays)
        <*> Toml.dioptional (Toml.text "bootstrap-prompt") .= (Just . agentBootstrapPrompt)
        <*> Toml.dioptional (Toml.text "title-prompt") .= (Just . agentTitlePrompt)
  where
    mkAgentConfig msbox msboxArgs mrunner mrunnerArgs mtimeout mlimit mretention mbootstrap mtitle =
        AgentConfig
            { agentSboxCommand = fromMaybe (agentSboxCommand defaultAgentConfig) msbox
            , agentSboxArgs = fromMaybe (agentSboxArgs defaultAgentConfig) msboxArgs
            , agentRunnerCommand = fromMaybe (agentRunnerCommand defaultAgentConfig) mrunner
            , agentRunnerArgs = fromMaybe (agentRunnerArgs defaultAgentConfig) mrunnerArgs
            , agentTimeoutSeconds = fromMaybe (agentTimeoutSeconds defaultAgentConfig) mtimeout
            , agentOutputLimitBytes = fromMaybe (agentOutputLimitBytes defaultAgentConfig) mlimit
            , agentSessionRetentionDays = fromMaybe (agentSessionRetentionDays defaultAgentConfig) mretention
            , agentBootstrapPrompt = fromMaybe (agentBootstrapPrompt defaultAgentConfig) mbootstrap
            , agentTitlePrompt = fromMaybe (agentTitlePrompt defaultAgentConfig) mtitle
            }

optionalAt :: (Toml.Key -> TomlCodec a) -> Toml.Key -> TomlCodec (Maybe a)
optionalAt codecAt key =
    Codec
        { codecRead = \toml -> if hasKey toml then Just <$> codecRead (codecAt key) toml else pure Nothing
        , codecWrite = traverse (codecWrite (codecAt key))
        }
  where
    hasKey toml =
        HashMap.member key (tomlPairs toml)
            || isJust (Prefix.lookup key (tomlTables toml))
            || isJust (implicitTables key (tomlTables toml))
            || HashMap.member key (tomlTableArrays toml)

tableAt :: TomlCodec a -> Toml.Key -> TomlCodec a
tableAt codec key =
    Codec
        { codecRead = \toml -> case implicitTables key (tomlTables toml) of
            Just children | isNothing (Prefix.lookup key (tomlTables toml)) -> codecRead codec mempty{tomlTables = children}
            _ -> codecRead (Toml.table codec key) toml
        , codecWrite = codecWrite (Toml.table codec key)
        }

implicitTables :: Toml.Key -> PrefixMap TOML -> Maybe (PrefixMap TOML)
implicitTables key@(piece :|| _) tables = HashMap.lookup piece tables >>= descend
  where
    descend (Leaf full value) = case keysDiff key full of
        FstIsPref rest -> Just (Prefix.single rest value)
        _ -> Nothing
    descend (Branch prefix value children) = case keysDiff key prefix of
        Equal -> Just children
        FstIsPref rest@(next :|| _) -> Just (HashMap.singleton next (Branch rest value children))
        SndIsPref rest -> implicitTables rest children
        _ -> Nothing

defaultedAt :: a -> (Toml.Key -> TomlCodec a) -> Toml.Key -> TomlCodec a
defaultedAt fallback codecAt key = Toml.dimap Just (fromMaybe fallback) (optionalAt codecAt key)

providersCodec :: TomlCodec (Map Text ProviderConfig)
providersCodec =
    Toml.dimap Map.elems (Map.fromList . map (\provider -> (providerName provider, provider))) (defaultedAt [] (Toml.list providerEntryCodec) "providers")
  where
    providerEntryCodec =
        ProviderConfig
            <$> Toml.text "name" .= providerName
            <*> Toml.text "base-url" .= providerBaseUrl
            <*> Toml.text "api-key-env" .= providerApiKeyEnv
            <*> defaultedAt 10 Toml.int "timeout-seconds" .= providerTimeoutSeconds

enumTextCodec :: (Text -> Maybe a) -> (a -> Text) -> Toml.Key -> TomlCodec a
enumTextCodec parse render =
    Toml.textBy render (\value -> maybe (Left ("Unknown value: " <> value)) Right (parse value))

piModelCodec :: TomlCodec PiModelConfig
piModelCodec =
    PiModelConfig
        <$> optionalAt Toml.text "name" .= piName
        <*> optionalAt Toml.int "contextWindow" .= piContextWindow
        <*> optionalAt Toml.int "maxTokens" .= piMaxTokens
        <*> defaultedAt [] (Toml.arrayOf Toml._Text) "input" .= piInput
        <*> optionalAt Toml.bool "reasoning" .= piReasoning
        <*> defaultedAt Map.empty (Toml.tableMap Toml._KeyText Toml.text) "thinkingLevelMap" .= piThinkingLevelMap
        <*> defaultedAt [] (Toml.arrayOf Toml._Text) "disabledThinkingLevels" .= piDisabledThinkingLevels
        <*> optionalAt (tableAt piCostCodec) "cost" .= piCost
        <*> defaultedAt emptyPiCompat (tableAt piCompatCodec) "compat" .= piCompat

piCostCodec :: TomlCodec PiCostConfig
piCostCodec =
    PiCostConfig
        <$> Toml.double "input" .= piCostInput
        <*> Toml.double "output" .= piCostOutput
        <*> Toml.double "cacheRead" .= piCostCacheRead
        <*> Toml.double "cacheWrite" .= piCostCacheWrite

piCompatCodec :: TomlCodec PiCompatConfig
piCompatCodec =
    PiCompatConfig
        <$> optionalAt Toml.bool "supportsStore" .= piSupportsStore
        <*> optionalAt Toml.bool "supportsDeveloperRole" .= piSupportsDeveloperRole
        <*> optionalAt Toml.bool "supportsReasoningEffort" .= piSupportsReasoningEffort
        <*> optionalAt Toml.bool "supportsUsageInStreaming" .= piSupportsUsageInStreaming
        <*> optionalAt Toml.bool "supportsFinishReason" .= piSupportsFinishReason
        <*> optionalAt Toml.text "maxTokensField" .= piMaxTokensField
        <*> optionalAt Toml.bool "requiresToolResultName" .= piRequiresToolResultName
        <*> optionalAt Toml.bool "requiresAssistantAfterToolResult" .= piRequiresAssistantAfterToolResult
        <*> optionalAt Toml.bool "requiresThinkingAsText" .= piRequiresThinkingAsText
        <*> optionalAt Toml.bool "requiresReasoningContentOnAssistantMessages" .= piRequiresReasoningContentOnAssistantMessages
        <*> optionalAt Toml.text "thinkingFormat" .= piThinkingFormat

chatModelCodec :: TomlCodec ChatModelConfig
chatModelCodec =
    ChatModelConfig
        <$> Toml.text "provider" .= chatProvider
        <*> enumTextCodec chatProtocolOf chatProtocolName "protocol" .= chatProtocol
        <*> Toml.text "model" .= chatModel
        <*> optionalAt Toml.text "thinking" .= chatThinking
        <*> defaultedAt emptyPiModel (tableAt piModelCodec) "pi" .= chatPi
  where
    chatProtocolOf name = case name of
        "openai-completions" -> Just ChatOpenAiCompletions
        "openai-responses" -> Just ChatOpenAiResponses
        "anthropic-messages" -> Just ChatAnthropicMessages
        "google-generative-ai" -> Just ChatGoogleGenerativeAi
        _ -> Nothing

embeddingProtocolCodec :: TomlCodec EmbeddingProtocol
embeddingProtocolCodec =
    enumTextCodec (\name -> if name == "openai-embeddings" then Just EmbeddingsOpenAi else Nothing) (const "openai-embeddings") "protocol"

judgeProtocolCodec :: TomlCodec JudgeProtocol
judgeProtocolCodec =
    enumTextCodec (\name -> if name == "systemone" then Just JudgeSystemOne else Nothing) (const "systemone") "protocol"

embeddingCodec :: TomlCodec EmbeddingConfig
embeddingCodec =
    EmbeddingConfig
        <$> Toml.text "provider" .= embeddingProvider
        <*> embeddingProtocolCodec .= embeddingProtocol
        <*> Toml.text "model" .= embeddingModel
        <*> optionalAt Toml.int "dimensions" .= embeddingDimensions
        <*> defaultedAt "{query}" Toml.text "query-template" .= embeddingQueryTemplate
        <*> defaultedAt "{document}" Toml.text "document-template" .= embeddingDocumentTemplate
        <*> defaultedAt 128 Toml.int "batch-size" .= embeddingBatchSize
        <*> defaultedAt 0.4 Toml.double "min-similarity" .= embeddingMinSimilarity

judgeCodec :: TomlCodec JudgeConfig
judgeCodec =
    JudgeConfig
        <$> Toml.text "provider" .= judgeProvider
        <*> judgeProtocolCodec .= judgeProtocol
        <*> Toml.text "model" .= judgeModel
        <*> defaultedAt "Is step {id} one of the steps the person searching for \"{query}\" wants to find?" Toml.text "instructions" .= judgeInstructions
        <*> defaultedAt 0.5 Toml.double "min-relevance" .= judgeMinRelevance

searchCodec :: TomlCodec SearchConfig
searchCodec =
    SearchConfig
        <$> defaultedAt 10 Toml.int "limit" .= searchLimit
        <*> defaultedAt 24 Toml.int "judge-candidates" .= searchJudgeCandidates
        <*> defaultedAt emptyEmbeddingConfig (tableAt embeddingCodec) "embedding" .= searchEmbedding
        <*> optionalAt (tableAt judgeCodec) "judge" .= searchJudge

configCodec :: TomlCodec Config
configCodec =
    mkConfig
        <$> Toml.table userRepoCodec "user-repo" .= configUserRepo
        <*> Toml.dimap Just (fromMaybe defaultSlurmConfig) (Toml.dioptional (Toml.table slurmCodec "slurm")) .= configSlurm
        <*> Toml.dioptional (Toml.table agentCodec "agent") .= (Just . configAgent)
        <*> providersCodec .= configProviders
        <*> optionalAt (tableAt chatModelCodec) "models.chat" .= configChat
        <*> optionalAt (tableAt searchCodec) "search" .= configSearch
  where
    mkConfig userRepo slurm mAgent providers chat search =
        Config userRepo slurm (fromMaybe defaultAgentConfig mAgent) providers chat search

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
    result <- Toml.decodeFileEither configCodec path
    case result of
        Left errs -> error $ "Failed to parse config: " ++ show errs
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
    filter (not . T.null . fst) $
        [ (chatProvider chat, "[models.chat]")
        | chat <- mapMaybe id [configChat config]
        ]
            ++ [ (embeddingProvider (searchEmbedding search), "[search.embedding]")
               | search <- mapMaybe id [configSearch config]
               ]
            ++ [ (judgeProvider judge, "[search.judge]")
               | search <- mapMaybe id [configSearch config]
               , judge <- mapMaybe id [searchJudge search]
               ]
