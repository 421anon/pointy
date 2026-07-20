{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Agent.Git (sweepStaleRunningSessions)
import Api (API)
import Config (loadConfig, resolveConfigPath)
import Control.Concurrent (forkIO)
import Control.Monad.Except (runExceptT)
import Handlers.Agent (
    archiveSessionHandler,
    confirmApplyHandler,
    createSessionHandler,
    discardSessionHandler,
    getSessionHandler,
    listSessionsHandler,
    postTurnHandler,
    prepareApplyHandler,
    purgeSessionHandler,
    renameSessionHandler,
    turnLogStreamHandler,
    usageHandler,
 )
import Handlers.Autocomplete (autocompleteHandler)
import Handlers.CommitHash (getCommitHashHandler)
import Handlers.Presets (getPresetsHandler)
import Handlers.ProjectEntities (assignRecordHandler, batchAssignRecordsHandler, unassignRecordHandler)
import Handlers.Projects (deleteProjectHandler, getProjectsHandler, patchProjectHandler, postProjectHandler)
import Handlers.RunStep (runStepHandler, stepLogHandler, stopStepHandler)
import Handlers.SrcFiles (downloadSrcFilesHandler, getUserRepoInfoHandler, listSrcFilesHandler, seekSrcFilesHandler)
import Handlers.StatusStream (stepStatusStreamHandler)
import Handlers.StepConfig (getStepConfigHandler)
import Handlers.Steps (noticesHandler, patchStepHandler, postStepHandler)
import Handlers.Store (stepBundleHandler, stepDownloadHandler, stepExtrasHandler, stepListHandler, stepRawHandler, stepSeekHandler)
import Handlers.Upload (uploadHandler)
import Network.Wai (Request, pathInfo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Network.Wai.Parse (setMaxRequestNumFiles)
import OutPaths (warmProjectOutPaths)
import Servant hiding (runHandler)
import Servant.Multipart
import System.IO (BufferMode (..), hSetBuffering, stdout)
import UserRepo (ensureUserRepo, fetchRepo)

server :: Server API
server =
    getCommitHashHandler
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
        :<|> getProjectsHandler
        :<|> postProjectHandler
        :<|> patchProjectHandler
        :<|> deleteProjectHandler
        :<|> assignRecordHandler
        :<|> batchAssignRecordsHandler
        :<|> unassignRecordHandler
        :<|> stepStatusStreamHandler
        :<|> getStepConfigHandler
        :<|> getPresetsHandler
        :<|> autocompleteHandler
        :<|> patchStepHandler
        :<|> postStepHandler
        :<|> noticesHandler
        :<|> runStepHandler
        :<|> stopStepHandler
        :<|> stepLogHandler
        :<|> uploadHandler
        :<|> createSessionHandler
        :<|> listSessionsHandler
        :<|> getSessionHandler
        :<|> postTurnHandler
        :<|> turnLogStreamHandler
        :<|> prepareApplyHandler
        :<|> confirmApplyHandler
        :<|> discardSessionHandler
        :<|> archiveSessionHandler
        :<|> renameSessionHandler
        :<|> purgeSessionHandler
        :<|> usageHandler

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
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["user-repo-info"] ->
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
    _ -> Nothing

app :: IO Application
app =
    let context = multipartOptions :. EmptyContext
     in pure $ cors corsPolicy $ serveWithContext (Proxy :: Proxy API) context server

multipartOptions :: MultipartOptions Tmp
multipartOptions =
    let opts = defaultMultipartOptions (Proxy :: Proxy Tmp)
        parserOpts = setMaxRequestNumFiles 100 (generalOptions opts)
     in opts{generalOptions = parserOpts}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Loading configuration..."
    configPath <- resolveConfigPath
    config <- loadConfig configPath
    putStrLn "Ensuring user repo is configured..."
    ensureUserRepo config
    putStrLn "Resetting stale agent runner state..."
    sweepStaleRunningSessions

    putStrLn "Fetching repository updates..."
    fetchResult <- runExceptT fetchRepo
    case fetchResult of
        Left err -> putStrLn $ "Warning: Failed to fetch repository: " ++ err
        Right () -> putStrLn "Repository fetched successfully."

    putStrLn "Starting server on port 8081..."
    application <- app
    let warmAfterServerStart = do
            putStrLn "Server listening on port 8081."
            _ <- forkIO $ do
                putStrLn "Warming project out paths..."
                warmProjectOutPaths
                putStrLn "Project out paths warmed."
            return ()
    runSettings (setPort 8081 $ setBeforeMainLoop warmAfterServerStart defaultSettings) application
