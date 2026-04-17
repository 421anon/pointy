{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config (..),
    UserRepoConfig (..),
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

data Config where
    Config :: {configUserRepo :: UserRepoConfig} -> Config
    deriving (Show)

userRepoCodec :: TomlCodec UserRepoConfig
userRepoCodec =
    UserRepoConfig
        <$> Toml.text "url" .= userRepoUrl
        <*> Toml.string "keyfile" .= userRepoKeyfile
        <*> Toml.text "branch" .= userRepoBranch

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table userRepoCodec "user-repo" .= configUserRepo

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
