module Handlers.StepConfig (getStepConfigHandler) where

import ApiTypes (DynamicJson (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.Encoding as TLE
import Data.ByteString (ByteString)
import Effects (AppM)
import Servant (throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), fetchRepo, runNixEvalJsonInRepo, withReadRepoTransaction)

getStepConfigHandler :: Maybe Text -> AppM DynamicJson
getStepConfigHandler mCommit = do
    result <- lift $ case mCommit of
        Just commit -> withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
            output <- runNixEvalJsonInRepo (ReadRepoContext repoPath $ unpack commit) "#pointy.stepConfig"
            return (LB.fromStrict output)
        Nothing -> withReadRepoTransaction $ \ctx -> do
            ExceptT $ liftIO $ runExceptT fetchRepo
            output <- runNixEvalJsonInRepo ctx "#pointy.stepConfig"
            return (LB.fromStrict output)
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
