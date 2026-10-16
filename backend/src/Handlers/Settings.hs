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
) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import ConfigWatch (readConfigText, replaceConfig)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
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
getUserRepoInfoHandler = liftIO $ do
    UserRepoConfig repoUrl keyfile repoBranch <- currentUserRepo
    UserRepoInfo repoUrl repoBranch <$> deployPublicKey keyfile

updateUserRepoHandler :: UserRepoUpdate -> AppM NoContent
updateUserRepoHandler update = do
    let newUrl = T.strip (updateUrl update)
        newBranch = T.strip (updateBranch update)
    when (T.null newUrl || T.null newBranch) $ badRequest "Enter both the repository URL and the branch."
    keyfile <- liftIO $ userRepoKeyfile <$> currentUserRepo
    orBadRequest (checkRepoAccess keyfile newUrl newBranch)
    contents <- liftIO readConfigText
    applied $ replaceConfig (withUserRepoTable (UserRepoConfig newUrl keyfile newBranch) contents)

getInstanceConfigHandler :: AppM Text
getInstanceConfigHandler = liftIO readConfigText

updateInstanceConfigHandler :: Text -> AppM NoContent
updateInstanceConfigHandler = applied . replaceConfig

currentUserRepo :: IO UserRepoConfig
currentUserRepo = configUserRepo <$> (resolveConfigPath >>= loadConfig)

applied :: IO (Either String ()) -> AppM NoContent
applied = (NoContent <$) . orBadRequest

orBadRequest :: IO (Either String a) -> AppM a
orBadRequest action = liftIO action >>= either badRequest pure

badRequest :: String -> AppM a
badRequest message = throwError err400{errBody = TLE.encodeUtf8 (TL.pack message)}

withUserRepoTable :: UserRepoConfig -> Text -> Text
withUserRepoTable repo contents = T.unlines (before ++ table ++ after)
  where
    (before, rest) = break ((== "[user-repo]") . T.strip) (T.lines contents)
    after = dropWhile (not . T.isPrefixOf "[" . T.stripStart) (drop 1 rest)
    table =
        [ "[user-repo]"
        , "url = " <> tomlString (userRepoUrl repo)
        , "keyfile = " <> tomlString (T.pack (userRepoKeyfile repo))
        , "branch = " <> tomlString (userRepoBranch repo)
        , ""
        ]
    tomlString value = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" value) <> "\""
