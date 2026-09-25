{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module App (serveApp, runServer, server, stripBackendPrefix) where

import Api (API)

import Effectful (Eff)
import Effects (AppEffects, AppM, toHandler)
import qualified EffectRunner
import EffectRunner (installRunner)
import Handlers.Agent (archiveSessionHandler, confirmApplyHandler, createSessionHandler, discardSessionHandler, getSessionHandler, listSessionsHandler, postTurnHandler, prepareApplyHandler, purgeSessionHandler, renameSessionHandler, steerTurnHandler, stopTurnHandler, turnLogStreamHandler, usageHandler)
import Handlers.Autocomplete (autocompleteHandler)
import Handlers.ClusterStream (clusterStatusStreamHandler, startClusterPoller)
import Handlers.CommitHash (getCommitHashHandler)
import Handlers.IngestStream (ingestStreamHandler)
import Handlers.Presets (getPresetsHandler)
import Handlers.ProjectEntities (assignRecordHandler, batchAssignRecordsHandler, unassignRecordHandler)
import Handlers.Projects (batchUpdateProjectsHandler, deleteProjectHandler, getProjectsHandler, patchProjectHandler, postProjectHandler)
import Handlers.RunStep (jobEndedHandler, restoreJobsFromSlurm, runStepHandler, stepLogHandler, stopStepHandler)
import Handlers.Scratch (scratchListHandler, scratchRootHandler, scratchWrapHandler)
import Handlers.SrcFiles (createSrcFileHandler, deleteSrcFileHandler, downloadSrcFilesHandler, getUserRepoInfoHandler, listSrcFilesHandler, saveSrcFileHandler, seekSrcFilesHandler, srcRawHandler)
import Handlers.StatusStream (projectStatusHandler, stepStatusStreamHandler)
import Handlers.Statuses (restoreRunningStatuses)
import Handlers.StepConfig (getStepConfigHandler)
import Handlers.StepReview (getProjectReviewHandler, removeReviewHandler, reviewDiffHandler, reviewStepHandler)
import Handlers.Steps (noticesHandler, patchStepHandler, postStepHandler)
import Handlers.Store (stepBundleHandler, stepDownloadHandler, stepExtrasHandler, stepListHandler, stepRawHandler, stepSeekHandler)
import Handlers.Upload (uploadHandler)
import Network.Wai (Application, Request, pathInfo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)

import Control.Monad.Except (mapExceptT)
import Control.Monad.IO.Class (liftIO)
import Servant (Context (..), Handler (..), Proxy (..), ServerT, hoistServer, serveWithContext, (:<|>) (..))
import Network.Wai.Parse (setMaxRequestNumFiles)
import Servant.Multipart (MultipartOptions, Tmp, TmpBackendOptions (..), backendOptions, defaultMultipartOptions, generalOptions)
import Storage (uploadStagingRoot)

server :: ServerT API AppM
server =
    liftHandler getCommitHashHandler
        :<|> getUserRepoInfoHandler
        :<|> stepListHandler
        :<|> stepDownloadHandler
        :<|> stepSeekHandler
        :<|> stepRawHandler
        :<|> stepBundleHandler
        :<|> stepExtrasHandler
        :<|> listSrcFilesHandler
        :<|> downloadSrcFilesHandler
        :<|> seekSrcFilesHandler
        :<|> srcRawHandler
        :<|> saveSrcFileHandler
        :<|> createSrcFileHandler
        :<|> deleteSrcFileHandler
        :<|> getProjectsHandler
        :<|> postProjectHandler
        :<|> patchProjectHandler
        :<|> batchUpdateProjectsHandler
        :<|> deleteProjectHandler
        :<|> assignRecordHandler
        :<|> batchAssignRecordsHandler
        :<|> unassignRecordHandler
        :<|> stepStatusStreamHandler
        :<|> projectStatusHandler
        :<|> getStepConfigHandler
        :<|> getPresetsHandler
        :<|> autocompleteHandler
        :<|> patchStepHandler
        :<|> postStepHandler
        :<|> getProjectReviewHandler
        :<|> reviewStepHandler
        :<|> removeReviewHandler
        :<|> reviewDiffHandler
        :<|> noticesHandler
        :<|> runStepHandler
        :<|> stopStepHandler
        :<|> stepLogHandler
        :<|> jobEndedHandler
        :<|> uploadHandler
        :<|> scratchRootHandler
        :<|> scratchListHandler
        :<|> scratchWrapHandler
        :<|> ingestStreamHandler
        :<|> clusterStatusStreamHandler
        :<|> liftHandler createSessionHandler
        :<|> liftHandler listSessionsHandler
        :<|> (\sessionId -> liftHandler (getSessionHandler sessionId))
        :<|> (\request -> liftHandler (postTurnHandler request))
        :<|> (\request -> liftHandler (stopTurnHandler request))
        :<|> (\request -> liftHandler (steerTurnHandler request))
        :<|> (\turnId -> liftHandler (turnLogStreamHandler turnId))
        :<|> (\request -> liftHandler (prepareApplyHandler request))
        :<|> (\request -> liftHandler (confirmApplyHandler request))
        :<|> (\request -> liftHandler (discardSessionHandler request))
        :<|> (\request -> liftHandler (archiveSessionHandler request))
        :<|> (\request -> liftHandler (renameSessionHandler request))
        :<|> (\request -> liftHandler (purgeSessionHandler request))
        :<|> liftHandler usageHandler

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case pathInfo req of
    ("agent" : _) ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type", "Last-Event-ID"]
                , corsMethods = ["GET", "POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["step-status-stream"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type", "Last-Event-ID"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Just (["http://localhost:3000"], True)
                }
    ["src-files"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["user-repo-info"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["src-files", "raw"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["src-files", "download"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["step-files"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["step-files", "download"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ("step-files" : _) ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["projects"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "POST", "PATCH", "DELETE", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["project-entities"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "DELETE", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["commit-hash"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["step-config"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["presets"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["autocomplete"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ("step" : _) ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "PATCH", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["run-step"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["stop-step"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["step-log"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["upload"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ("scratch" : _) ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["ingest-stream"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type", "Last-Event-ID"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["cluster-status-stream"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type", "Last-Event-ID"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    _ -> Nothing

multipartOptions :: MultipartOptions Tmp
multipartOptions =
    let opts = defaultMultipartOptions (Proxy :: Proxy Tmp)
        parserOpts = setMaxRequestNumFiles 100 (generalOptions opts)
        backendOpts = backendOptions opts
     in opts{generalOptions = parserOpts, backendOptions = backendOpts{getTmpDir = uploadStagingRoot}}

stripBackendPrefix :: Application -> Application
stripBackendPrefix application request respond =
    case pathInfo request of
        ("backend" : rest) -> application request{pathInfo = rest} respond
        _ -> application request respond

liftHandler :: Handler a -> AppM a
liftHandler (Handler action) = mapExceptT liftIO action

serveApp :: (forall x. Eff AppEffects x -> IO x) -> (Application -> Application) -> Application
serveApp runEffects middleware =
    middleware $
        cors corsPolicy $
            serveWithContext
                (Proxy :: Proxy API)
                (multipartOptions :. EmptyContext)
                (hoistServer (Proxy :: Proxy API) (toHandler runEffects) server)


runServer :: (forall x. Eff AppEffects x -> IO x) -> (Application -> Application) -> Int -> IO () -> IO ()
runServer runEffects middleware port warm = do
    installRunner (EffectRunner.Runner runEffects)
    runSettings (setPort port (setBeforeMainLoop warm defaultSettings)) (serveApp runEffects middleware)
