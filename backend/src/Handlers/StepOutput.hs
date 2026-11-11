{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepOutput (resolveStepOutPath, stepOutPathAt) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effectful (UnliftStrategy (SeqUnlift), withEffToIO)
import Effects (AppM)
import Servant (ServerError (..), err500, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), runNixEvalRawInRepo, userRepoPath, withReadRepoTransaction)

{-# NOINLINE outPathsAtCommit #-}
outPathsAtCommit :: SingleFlightCache (Int, Text) Text
outPathsAtCommit = unsafePerformIO (newSingleFlightCache 256)

stepOutPathAt :: Int -> Text -> AppM Text
stepOutPathAt stepId commit =
    ExceptT $
        withEffToIO SeqUnlift $ \unlift ->
            cachedSuccess outPathsAtCommit (stepId, commit) (unlift (runExceptT (resolveStepOutPath stepId (Just commit))))

resolveStepOutPath :: Int -> Maybe Text -> AppM Text
resolveStepOutPath stepId mCommit = do
    repoPath <- liftIO userRepoPath
    commitHash <- case mCommit of
        Just c -> return (T.unpack c)
        Nothing -> do
            result <- lift $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return hash
            case result of
                Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("resolveStepOutPath: " ++ err))}
                Right h -> return h
    let ctx = ReadRepoContext repoPath commitHash
    result <- lift $ runExceptT $ runNixEvalRawInRepo ctx ("#pointy.steps." ++ show stepId ++ ".outPath")
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to resolve step outPath: " ++ err))}
        Right path -> return (T.pack path)
