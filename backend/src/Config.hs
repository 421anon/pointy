{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (..),
    UserRepoConfig (..),
    SlurmConfig (..),
    AgentConfig (..),
    defaultAgentConfig,
    loadConfig,
    defaultConfigPath,
    resolveConfigPath,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml

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
    }
    deriving (Show)

defaultAgentConfig :: AgentConfig
defaultAgentConfig =
    AgentConfig
        { agentSboxCommand = "sbox"
        , agentSboxArgs = ["--bind", "{home}", "{home}"]
        , agentRunnerCommand = "pi"
        , agentRunnerArgs = ["-p", "-c", "{prompt}"]
        , agentTimeoutSeconds = 1800
        , agentOutputLimitBytes = 1048576
        , agentSessionRetentionDays = 7
        }

data Config where
    Config :: {configUserRepo :: UserRepoConfig, configSlurm :: SlurmConfig, configAgent :: AgentConfig} -> Config
    deriving (Show)

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
  where
    mkAgentConfig msbox msboxArgs mrunner mrunnerArgs mtimeout mlimit mretention =
        AgentConfig
            { agentSboxCommand = fromMaybe (agentSboxCommand defaultAgentConfig) msbox
            , agentSboxArgs = fromMaybe (agentSboxArgs defaultAgentConfig) msboxArgs
            , agentRunnerCommand = fromMaybe (agentRunnerCommand defaultAgentConfig) mrunner
            , agentRunnerArgs = fromMaybe (agentRunnerArgs defaultAgentConfig) mrunnerArgs
            , agentTimeoutSeconds = fromMaybe (agentTimeoutSeconds defaultAgentConfig) mtimeout
            , agentOutputLimitBytes = fromMaybe (agentOutputLimitBytes defaultAgentConfig) mlimit
            , agentSessionRetentionDays = fromMaybe (agentSessionRetentionDays defaultAgentConfig) mretention
            }


configCodec :: TomlCodec Config
configCodec =
    mkConfig
        <$> Toml.table userRepoCodec "user-repo" .= configUserRepo
        <*> Toml.dimap Just (fromMaybe defaultSlurmConfig) (Toml.dioptional (Toml.table slurmCodec "slurm")) .= configSlurm
        <*> Toml.dioptional (Toml.table agentCodec "agent") .= (Just . configAgent)
  where
    mkConfig userRepo slurm mAgent = Config userRepo slurm (fromMaybe defaultAgentConfig mAgent)

defaultConfigPath :: FilePath
defaultConfigPath = "/home/backend/config.toml"

resolveConfigPath :: IO FilePath
resolveConfigPath = fromMaybe defaultConfigPath <$> lookupEnv "POINTY_CONFIG_PATH"

loadConfig :: FilePath -> IO Config
loadConfig path = do
    result <- Toml.decodeFileEither configCodec path
    case result of
        Left errs -> error $ "Failed to parse config: " ++ show errs
        Right config -> return config
