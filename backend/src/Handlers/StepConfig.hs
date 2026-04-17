module Handlers.StepConfig (getStepConfigHandler) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Servant (Handler, throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), fetchRepo, runNixInRepo, withReadRepoTransaction)

getStepConfigHandler :: Maybe Text -> Handler LBS.ByteString
getStepConfigHandler mCommit = do
    result <- liftIO $ case mCommit of
        Just commit -> withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
            output <- runNixInRepo (ReadRepoContext repoPath $ unpack commit) ["eval", "--json"] "#trotter.stepConfig"
            return (TLE.encodeUtf8 (TL.pack output))
        Nothing -> withReadRepoTransaction $ \ctx -> do
            fetchRepo
            output <- runNixInRepo ctx ["eval", "--json"] "#trotter.stepConfig"
            return (TLE.encodeUtf8 (TL.pack output))
    case result of
        Right output -> return output
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
