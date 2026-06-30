module Handlers.Presets (getPresetsHandler) where

import ApiTypes (DynamicJson (..))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Servant (Handler, throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, withReadRepoTransaction)

getPresetsHandler :: Maybe Text -> Handler DynamicJson
getPresetsHandler mCommit = do
    result <- liftIO $ case mCommit of
        Just commit -> withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
            output <- runNixEvalJsonInRepo (ReadRepoContext repoPath $ unpack commit) "#pointy.presets"
            return (TLE.encodeUtf8 (TL.pack output))
        Nothing -> withReadRepoTransaction $ \ctx -> do
            output <- runNixEvalJsonInRepo ctx "#pointy.presets"
            return (TLE.encodeUtf8 (TL.pack output))
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
