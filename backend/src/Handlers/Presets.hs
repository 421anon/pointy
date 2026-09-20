module Handlers.Presets (getPresetsHandler) where

import ApiTypes (DynamicJson (..))
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import Servant (throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), runNixEvalJsonApplyInRepo, withReadRepoTransaction)

getPresetsHandler :: Maybe Text -> AppM DynamicJson
getPresetsHandler mCommit = do
    let evalPresets ctx = do
            output <- runNixEvalJsonApplyInRepo ctx "pointy: pointy.presets or {}" "#pointy"
            return (TLE.encodeUtf8 (TL.pack output))
    result <- lift $ case mCommit of
        Just commit -> withReadRepoTransaction $ \(ReadRepoContext repoPath _) ->
            evalPresets (ReadRepoContext repoPath $ unpack commit)
        Nothing -> withReadRepoTransaction evalPresets
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
