module Handlers.CommitHash (getCommitHashHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Servant (Handler, throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

getCommitHashHandler :: Handler Text
getCommitHashHandler = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ commitHash) ->
        return commitHash
    case result of
        Right hash -> return (pack hash)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
