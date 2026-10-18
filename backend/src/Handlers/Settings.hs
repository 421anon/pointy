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
import ConfigWatch (modifyConfig, readConfigText)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), object, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import GHC.Generics (Generic)
import Servant (NoContent (..), ServerError (..), err400, throwError)
import Toml.Syntax (Expr (..), Key, Located (..), Position (..), parseRawToml)
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
    UserRepoConfig repoUrl repoBranch <- savedUserRepo
    liftIO $ UserRepoInfo repoUrl repoBranch <$> deployPublicKey

updateUserRepoHandler :: UserRepoUpdate -> AppM NoContent
updateUserRepoHandler update = do
    let newUrl = T.strip (updateUrl update)
        newBranch = T.strip (updateBranch update)
    when (T.null newUrl || T.null newBranch) $ badRequest "Enter both the repository URL and the branch."
    orBadRequest (checkRepoAccess newUrl newBranch)
    applied $ modifyConfig (withUserRepoTable (UserRepoConfig newUrl newBranch))

getInstanceConfigHandler :: AppM Text
getInstanceConfigHandler = liftIO readConfigText

updateInstanceConfigHandler :: Text -> AppM NoContent
updateInstanceConfigHandler = applied . modifyConfig . const . Right

getConfigFormHandler :: AppM Value
getConfigFormHandler = do
    config <- savedConfig "The configuration can't be shown as a form: "
    pure $ object ["fields" .= filter ((/= "user-repo") . fieldKey) configFields, "values" .= withoutUserRepo (configJson config)]
  where
    withoutUserRepo (Object entries) = Object (KeyMap.delete "user-repo" entries)
    withoutUserRepo other = other

updateConfigFormHandler :: Value -> AppM NoContent
updateConfigFormHandler values =
    applied $ modifyConfig $ \contents -> do
        saved <- first (savedConfigError ++) (decodeToml contents)
        renderConfig <$> decodeConfigJson (withUserRepoOf saved values)
  where
    withUserRepoOf (Object saved) (Object submitted) = Object (maybe submitted (\repo -> KeyMap.insert "user-repo" repo submitted) (KeyMap.lookup "user-repo" saved))
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

withUserRepoTable :: UserRepoConfig -> Text -> Either String Text
withUserRepoTable repo contents = do
    exprs <- first ((savedConfigError ++) . locThing) (parseRawToml contents)
    case break snd (mapMaybe header exprs) of
        (_, (start, _) : later) ->
            let next = maybe (length fileLines + 1) fst (listToMaybe later)
                body = take (next - start - 1) (drop start fileLines)
                trailingTrivia = reverse (takeWhile isTrivia (reverse body))
             in Right (T.unlines (take (start - 1) fileLines ++ T.lines (T.stripEnd (renderUserRepo repo)) ++ trailingTrivia ++ drop (next - 1) fileLines))
        _ -> Left "The configuration doesn't write [user-repo] as a table of its own, so change it under Instance configuration."
  where
    fileLines = T.lines contents
    header expr = case expr of
        TableExpr key -> Just (keyLine key, map snd (NonEmpty.toList key) == ["user-repo"])
        ArrayTableExpr key -> Just (keyLine key, False)
        KeyValExpr _ _ -> Nothing
    isTrivia line = T.null (T.strip line) || "#" `T.isPrefixOf` T.strip line

keyLine :: Key Position -> Int
keyLine = posLine . fst . NonEmpty.head
