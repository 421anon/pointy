{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.ArtifactRdf (artifactRdfHandler) where

import ArtifactRdf (ArtifactRef (..), closureExpression, closureTurtle, parseArtifactSegments, stepNodesFromClosure)
import Certificates (evalProjectDefinitions)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Value (..), eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import ProjectTree (stepFolderPaths)
import Servant (Header, Headers, ServerError (..), addHeader, err404, err500)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (commitContext, ensureRepoCommit, runNixEvalJsonApplyInRepo, userRepoPath)

type ArtifactRdfResponse = Headers '[Header "Cache-Control" Text, Header "Vary" Text] Text

artifactRdfHandler :: [Text] -> AppM ArtifactRdfResponse
artifactRdfHandler segments = do
    ref <- maybe (throwError err404{errBody = "Expected /artifact/<project path>/<step id>/<commit>"}) pure (parseArtifactSegments segments)
    cached <- liftIO (lookup ref <$> readMVar artifactRdfCacheRef)
    turtle <- maybe (describe ref) pure cached
    pure (addHeader "public, max-age=31536000, immutable" (addHeader "Accept" turtle))

describe :: ArtifactRef -> AppM Text
describe ref = do
    resolved <- liftIO $ runExceptT $ do
        ensureRepoCommit (T.unpack (refCommit ref))
        repoPath <- liftIO userRepoPath
        commitContext repoPath (refCommit ref)
    ctx <- either (\err -> throwError err404{errBody = utf8 err}) pure resolved
    evaluated <- lift $ runExceptT $ do
        projects <- evalProjectDefinitions ctx
        closure <- runNixEvalJsonApplyInRepo ctx (closureExpression (refStepId ref)) "#pointy.steps"
        decoded <- ExceptT (pure (eitherDecode (LBS.fromStrict (TE.encodeUtf8 (T.pack closure)))))
        pure (projects, decoded)
    (projects, closure) <- either (\err -> throwError err500{errBody = utf8 ("Failed to evaluate the step closure: " ++ err)}) pure evaluated
    nodes <- case closure of
        Null -> throwError err404{errBody = utf8 ("Step " ++ show (refStepId ref) ++ " does not exist at " ++ T.unpack (refCommit ref))}
        value -> maybe (throwError err500{errBody = "Unexpected step closure shape"}) pure (stepNodesFromClosure value)
    let turtle = closureTurtle ref (stepFolderPaths projects) nodes
    liftIO $ modifyMVar_ artifactRdfCacheRef $ \entries ->
        pure (take artifactRdfCacheLimit ((ref, turtle) : filter ((/= ref) . fst) entries))
    pure turtle

utf8 :: String -> LBS.ByteString
utf8 = TLE.encodeUtf8 . TL.pack

{-# NOINLINE artifactRdfCacheRef #-}
artifactRdfCacheRef :: MVar [(ArtifactRef, Text)]
artifactRdfCacheRef = unsafePerformIO (newMVar [])

artifactRdfCacheLimit :: Int
artifactRdfCacheLimit = 256
