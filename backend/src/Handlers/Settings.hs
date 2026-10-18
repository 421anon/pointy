{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Settings (
    UserRepoInfo (..),
    UserRepoUpdate (..),
    getUserRepoInfoHandler,
    updateUserRepoHandler,
    getInstanceConfigHandler,
    updateInstanceConfigHandler,
    getConfigFormHandler,
    updateConfigFormHandler,
) where

import Config (Config (..), UserRepoConfig (..), configFields, configJson, decodeConfig, decodeConfigJson, renderConfig, renderUserRepo)
import ConfigSchema (ConfigField (..), decodeToml)
import ConfigWatch (readConfigText, replaceConfig)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), object, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import GHC.Generics (Generic)
import Servant (NoContent (..), ServerError (..), err400, throwError)
import UserRepo (checkRepoAccess, deployPublicKey)

data UserRepoInfo = UserRepoInfo
    { url :: Text
    , branch :: Text
    , publicKey :: Maybe Text
    }
    deriving (Generic, ToJSON)

data UserRepoUpdate = UserRepoUpdate
    { updateUrl :: Text
    , updateBranch :: Text
    }

instance FromJSON UserRepoUpdate where
    parseJSON = withObject "UserRepoUpdate" $ \object_ -> UserRepoUpdate <$> object_ .: "url" <*> object_ .: "branch"

getUserRepoInfoHandler :: AppM UserRepoInfo
getUserRepoInfoHandler = do
    UserRepoConfig repoUrl keyfile repoBranch <- savedUserRepo
    liftIO $ UserRepoInfo repoUrl repoBranch <$> deployPublicKey keyfile

updateUserRepoHandler :: UserRepoUpdate -> AppM NoContent
updateUserRepoHandler update = do
    let newUrl = T.strip (updateUrl update)
        newBranch = T.strip (updateBranch update)
    when (T.null newUrl || T.null newBranch) $ badRequest "Enter both the repository URL and the branch."
    keyfile <- userRepoKeyfile <$> savedUserRepo
    orBadRequest (checkRepoAccess keyfile newUrl newBranch)
    contents <- liftIO readConfigText
    applied $ replaceConfig (withUserRepoTable (UserRepoConfig newUrl keyfile newBranch) contents)

getInstanceConfigHandler :: AppM Text
getInstanceConfigHandler = liftIO readConfigText

updateInstanceConfigHandler :: Text -> AppM NoContent
updateInstanceConfigHandler = applied . replaceConfig

getConfigFormHandler :: AppM Value
getConfigFormHandler = do
    config <- savedConfig "The configuration can't be shown as a form: "
    pure $ object ["fields" .= filter ((/= "user-repo") . fieldKey) configFields, "values" .= withoutUserRepo (configJson config)]
  where
    withoutUserRepo (Object entries) = Object (KeyMap.delete "user-repo" entries)
    withoutUserRepo other = other

updateConfigFormHandler :: Value -> AppM NoContent
updateConfigFormHandler values = do
    current <- liftIO readConfigText >>= either (badRequest . (savedConfigError ++)) pure . decodeToml
    config <- either badRequest pure (decodeConfigJson (withUserRepoOf current values))
    applied (replaceConfig (renderConfig config))
  where
    withUserRepoOf (Object current) (Object submitted) = Object (maybe submitted (\repo -> KeyMap.insert "user-repo" repo submitted) (KeyMap.lookup "user-repo" current))
    withUserRepoOf _ submitted = submitted

savedConfig :: String -> AppM Config
savedConfig context = liftIO readConfigText >>= either (badRequest . (context ++)) pure . decodeConfig

savedUserRepo :: AppM UserRepoConfig
savedUserRepo = configUserRepo <$> savedConfig savedConfigError

savedConfigError :: String
savedConfigError = "The configuration on disk can't be read: "

applied :: IO (Either String ()) -> AppM NoContent
applied = (NoContent <$) . orBadRequest

orBadRequest :: IO (Either String a) -> AppM a
orBadRequest action = liftIO action >>= either badRequest pure

badRequest :: String -> AppM a
badRequest message = throwError err400{errBody = TLE.encodeUtf8 (TL.pack message)}

withUserRepoTable :: UserRepoConfig -> Text -> Text
withUserRepoTable repo contents = T.unlines (before ++ T.lines (renderUserRepo repo) ++ after)
  where
    (before, rest) = break ((== "[user-repo]") . T.strip) (T.lines contents)
    after = dropWhile (not . T.isPrefixOf "[" . T.stripStart) (drop 1 rest)
