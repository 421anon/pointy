{-# LANGUAGE OverloadedStrings #-}

module Handlers.ProjectStatus (projectStatusHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import Handlers.Statuses (broadcastProjectStatus, forkReporting)
import Servant (NoContent (..), throwError)
import Servant.Server (err500, errBody)
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

projectStatusHandler :: Int -> Maybe Text -> AppM NoContent
projectStatusHandler projectId commit = do
    targetCommit <-
        case commit of
            Just c -> pure c
            Nothing -> do
                result <- lift $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> pure (pack hash)
                either (\err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}) pure result
    liftIO $
        forkReporting
            ("Project status broadcast for " ++ show projectId)
            (broadcastProjectStatus projectId targetCommit Nothing)
    pure NoContent
