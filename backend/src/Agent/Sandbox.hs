{-# LANGUAGE OverloadedStrings #-}

module Agent.Sandbox (
    sessionHome,
    bindPath,
    bindPathReadOnly,
    nixDaemonBindArgs,
    piAgentConfigDir,
    runnerCommand,
    runnerEnvironment,
    agentMemoryMax,
    prepareAgentCgroup,
    sandboxCommand,
    sandboxProcess,
) where

import Agent.Session (AgentSession (..))
import Config (ChatModelConfig (..), Config (..), ProviderConfig (..), loadConfig, resolveConfigPath)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NixStore (NixStore (..), nixStore)
import Storage (scratchDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesPathExist, getHomeDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess, proc)

nixCompatSocket :: FilePath
nixCompatSocket = "/run/nix-daemon-socket"

nixDaemonBindArgs :: IO [String]
nixDaemonBindArgs = do
    compatArgs <- compatBindArgs
    remoteArgs <- remoteBindArgs
    scratchArgs <- maybe [] bindPathReadOnly <$> scratchDirectory
    return (compatArgs ++ remoteArgs ++ scratchArgs)
  where
    compatBindArgs = do
        exists <- doesFileExist nixCompatSocket
        return $
            if exists
                then ["--bind", nixCompatSocket, "/var/run/nix-daemon-socket"]
                else []
    remoteBindArgs = case storeSocketPath nixStore of
        Nothing -> pure []
        Just socket -> do
            socketExists <- doesPathExist socket
            rootArgs <- case storeRoot nixStore of
                Nothing -> pure []
                Just root -> do
                    rootExists <- doesPathExist root
                    pure (if rootExists then ["--ro-bind", root, root] else [])
            pure ((if socketExists then ["--bind", socket, socket] else []) ++ rootArgs)

bindPath :: FilePath -> [String]
bindPath path = ["--bind", path, path]

bindPathReadOnly :: FilePath -> [String]
bindPathReadOnly path = ["--ro-bind", path, path]

piAgentConfigDir :: IO FilePath
piAgentConfigDir = (\home -> home </> ".pi" </> "agent") <$> getHomeDirectory

sessionHome :: AgentSession -> FilePath
sessionHome session_ = takeDirectory (worktreePath session_) </> "home"

sandboxCommand :: FilePath
sandboxCommand = "sbox"

runnerCommand :: String
runnerCommand = "pi"

runnerEnvironment :: [(String, String)] -> IO [(String, String)]
runnerEnvironment ownVars = do
    baseEnv <- getEnvironment
    chatKeyEnv <- chatApiKeyEnv
    return $
        ("PATH", fromMaybe fallbackPath (lookup "PATH" baseEnv))
            : ownVars
            ++ remoteVars
            ++ filter ((`elem` passthroughKeys chatKeyEnv) . fst) baseEnv
  where
    fallbackPath = "/run/current-system/sw/bin:/usr/bin:/bin"
    remoteVars = maybe [] (\remote -> [("NIX_REMOTE", remote)]) (storeRemote nixStore)
    passthroughKeys chatKey =
        [ "USER"
        , "LOGNAME"
        , "SHELL"
        , "TERM"
        , "LANG"
        , "LC_ALL"
        , "TZ"
        , "XDG_RUNTIME_DIR"
        , "XDG_DATA_DIRS"
        ]
            ++ maybe [] (\key -> [key]) chatKey

chatApiKeyEnv :: IO (Maybe String)
chatApiKeyEnv = do
    config <- resolveConfigPath >>= loadConfig
    case configChat config of
        Nothing -> pure Nothing
        Just chat -> case Map.lookup (chatProvider chat) (configProviders config) of
            Nothing -> pure Nothing
            Just provider -> pure (Just (T.unpack (providerApiKeyEnv provider)))

agentMemoryMax :: IO (Maybe Text)
agentMemoryMax = fmap T.pack <$> lookupEnv "POINTY_AGENT_MEMORY_MAX"

agentCgroup :: IO FilePath
agentCgroup = do
    membership <- lines <$> readFile "/proc/self/cgroup"
    case mapMaybe (stripPrefix "0::") membership of
        [ownCgroup] -> return ("/sys/fs/cgroup" ++ takeDirectory ownCgroup </> "agents")
        _ -> ioError (userError "the backend is not running in a cgroup v2 hierarchy")

prepareAgentCgroup :: IO ()
prepareAgentCgroup = agentMemoryMax >>= mapM_ limitAgents
  where
    limitAgents memoryMax = do
        cgroup <- agentCgroup
        writeFile (takeDirectory cgroup </> "cgroup.subtree_control") "+memory"
        createDirectoryIfMissing False cgroup
        writeFile (cgroup </> "memory.max") (T.unpack memoryMax)

sandboxProcess :: [String] -> IO CreateProcess
sandboxProcess args = do
    memoryMax <- agentMemoryMax
    case memoryMax of
        Nothing -> return (proc sandboxCommand args)
        Just _ -> do
            cgroup <- agentCgroup
            return (proc "bash" (["-c", "echo $$ > \"$0/cgroup.procs\" && exec \"$@\"", cgroup, sandboxCommand] ++ args))
