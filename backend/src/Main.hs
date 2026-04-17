{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Config (loadConfig, resolveConfigPath)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Text (Text)
import Handlers.CommitHash (getCommitHashHandler)
import Handlers.ProjectEntities (assignRecordHandler, batchAssignRecordsHandler, unassignRecordHandler)
import Handlers.Projects (RawJSON, deleteProjectHandler, getProjectsHandler, patchProjectHandler, postProjectHandler)
import Handlers.RunStep (runStepHandler, stopStepHandler)
import Handlers.SrcFiles (UserRepoInfo, downloadSrcFilesHandler, getUserRepoInfoHandler, listSrcFilesHandler)
import Handlers.StatusStream (EventStream, stepStatusStreamHandler)
import Handlers.Statuses (getProjectOutPathsHandler)
import Handlers.StepConfig (getStepConfigHandler)
import Handlers.Steps (patchStepHandler, postStepHandler)
import Handlers.Store (DirEntry, downloadHandler, listHandler, storeFilesHandler)
import Handlers.Upload (uploadHandler)
import Network.Wai (Request, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import OutPaths (cacheProjectOutPaths)
import Servant hiding (runHandler)
import Servant.Multipart
import Servant.Types.SourceT (SourceT)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import UserRepo (ensureUserRepo, fetchRepo)

type API =
    "hello" :> Get '[PlainText] Text
        :<|> "commit-hash" :> Get '[PlainText] Text
        :<|> "user-repo-info" :> Get '[JSON] UserRepoInfo
        :<|> "store" :> QueryParam' '[Required] "outPath" Text :> QueryParam "path" FilePath :> Get '[JSON] [DirEntry]
        :<|> "store" :> "download" :> QueryParam' '[Required] "outPath" Text :> QueryParam' '[Required] "path" FilePath :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (SourceT IO BS.ByteString))
        :<|> "store-files" :> CaptureAll "segments" String :> Raw
        :<|> "src-files" :> QueryParam' '[Required, Strict] "id" Int :> QueryParam "path" FilePath :> Get '[JSON] [DirEntry]
        :<|> "src-files" :> "download" :> QueryParam' '[Required, Strict] "id" Int :> QueryParam' '[Required] "path" FilePath :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (SourceT IO BS.ByteString))
        :<|> "projects" :> QueryParam "commit" Text :> Get '[RawJSON] LBS.ByteString
        :<|> "projects" :> ReqBody '[RawJSON] LBS.ByteString :> Post '[RawJSON] LBS.ByteString
        :<|> "projects" :> QueryParam' '[Required, Strict] "id" Int :> ReqBody '[RawJSON] LBS.ByteString :> Patch '[JSON] NoContent
        :<|> "projects" :> QueryParam' '[Required, Strict] "id" Int :> Delete '[JSON] NoContent
        :<|> "project-entities" :> QueryParam' '[Required, Strict] "project_id" Int :> QueryParam' '[Required, Strict] "entity_id" Int :> Post '[JSON] NoContent
        :<|> "project-entities" :> "batch" :> QueryParam' '[Required, Strict] "project_id" Int :> ReqBody '[JSON] [Int] :> Post '[JSON] NoContent
        :<|> "project-entities" :> QueryParam' '[Required, Strict] "project_id" Int :> QueryParam' '[Required, Strict] "entity_id" Int :> Delete '[JSON] NoContent
        :<|> "project-out-paths" :> QueryParam' '[Required, Strict] "id" Int :> QueryParam "commit" Text :> Get '[JSON] (Map Int Text)
        :<|> "step-status-stream" :> QueryParam' '[Required, Strict] "project_id" Int :> QueryParam "commit" Text :> StreamGet NoFraming EventStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceT IO BS.ByteString))
        :<|> "step-config" :> QueryParam "commit" Text :> Get '[RawJSON] LBS.ByteString
        :<|> "step" :> QueryParam' '[Required, Strict] "id" Int :> ReqBody '[RawJSON] LBS.ByteString :> Patch '[JSON] NoContent
        :<|> "step" :> QueryParam "project_id" Int :> ReqBody '[RawJSON] LBS.ByteString :> Post '[RawJSON] LBS.ByteString
        :<|> "run-step" :> QueryParam' '[Required, Strict] "id" Int :> QueryParam "commit" Text :> Post '[PlainText] NoContent
        :<|> "stop-step" :> QueryParam' '[Required, Strict] "id" Int :> QueryParam "commit" Text :> Post '[PlainText] NoContent
        :<|> "upload" :> QueryParam' '[Required, Strict] "id" Int :> MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] Text

server :: Server API
server =
    return "Hello, World!"
        :<|> getCommitHashHandler
        :<|> getUserRepoInfoHandler
        :<|> listHandler
        :<|> downloadHandler
        :<|> storeFilesHandler
        :<|> listSrcFilesHandler
        :<|> downloadSrcFilesHandler
        :<|> getProjectsHandler
        :<|> postProjectHandler
        :<|> patchProjectHandler
        :<|> deleteProjectHandler
        :<|> assignRecordHandler
        :<|> batchAssignRecordsHandler
        :<|> unassignRecordHandler
        :<|> getProjectOutPathsHandler
        :<|> stepStatusStreamHandler
        :<|> getStepConfigHandler
        :<|> patchStepHandler
        :<|> postStepHandler
        :<|> runStepHandler
        :<|> stopStepHandler
        :<|> uploadHandler

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case pathInfo req of
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
    ["store"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "DELETE", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["store", "download"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ("store-files" : _) ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
                , corsOrigins = Nothing
                }
    ["store", "readcount"] ->
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
    ["project-out-paths"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["GET", "OPTIONS"]
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
    ["upload"] ->
        Just $
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"]
                , corsMethods = ["POST", "OPTIONS"]
                , corsOrigins = Nothing
                }
    _ -> Nothing

app :: IO Application
app = do
    multipartOpts <- customMultipartOptions
    let context = multipartOpts :. EmptyContext
    return $ cors corsPolicy $ serveWithContext (Proxy :: Proxy API) context server

customMultipartOptions :: IO (MultipartOptions Tmp)
customMultipartOptions = do
    homeDir <- getHomeDirectory
    let tmpDir = homeDir </> "tmp"
        tmpBackendOpts =
            TmpBackendOptions
                { getTmpDir = return tmpDir
                , filenamePat = "upload_*.tmp"
                }
        opts = defaultMultipartOptions (Proxy :: Proxy Tmp)
    return $ opts{backendOptions = tmpBackendOpts}

ensureStoreDirectories :: IO ()
ensureStoreDirectories = do
    homeDir <- getHomeDirectory
    createDirectoryIfMissing True (homeDir </> "tmp")

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Loading configuration..."
    configPath <- resolveConfigPath
    config <- loadConfig configPath
    putStrLn "Ensuring user repo is configured..."
    ensureUserRepo config
    putStrLn "Ensuring store directories exist..."
    ensureStoreDirectories

    putStrLn "Fetching repository updates..."
    fetchResult <- runExceptT fetchRepo
    case fetchResult of
        Left err -> putStrLn $ "Warning: Failed to fetch repository: " ++ err
        Right () -> putStrLn "Repository fetched successfully."

    putStrLn "Caching project out paths..."
    cacheProjectOutPaths
    putStrLn "Caching complete."

    putStrLn "Starting server on port 8081..."
    application <- app
    run 8081 application
