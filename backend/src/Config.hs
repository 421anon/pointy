{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (..),
    UserRepoConfig (..),
    SlurmConfig (..),
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

data Config where
    Config :: {configUserRepo :: UserRepoConfig, configSlurm :: SlurmConfig} -> Config
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
configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table userRepoCodec "user-repo" .= configUserRepo
        <*> Toml.dimap Just (fromMaybe defaultSlurmConfig) (Toml.dioptional (Toml.table slurmCodec "slurm")) .= configSlurm

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
