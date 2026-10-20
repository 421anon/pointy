{-# LANGUAGE OverloadedStrings #-}

module Agent.Sandbox (
    SandboxPaths (..),
    sessionPaths,
    expandSandboxArg,
    bindPath,
    bindPathReadOnly,
    nixDaemonBindArgs,
    piAgentConfigDir,
    piIsolationFlags,
    seedPiConfig,
    runnerEnvironment,
    toolEnvironment,
    runnerConfigArgs,
    agentMemoryMax,
    prepareAgentCgroup,
    sandboxProcess,
) where

import Agent.Session (AgentSession (..))
import Config (AgentConfig (..), ChatModelConfig (..), Config (..), ProviderConfig (..), loadConfig, resolveConfigPath)
import Control.Monad (filterM, forM_)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NixStore (NixStore (..), nixStore)
import Storage (scratchDirectory)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, getHomeDirectory)
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

piIsolationFlags :: [String]
piIsolationFlags = ["--no-context-files", "--no-skills", "--no-extensions", "--no-prompt-templates", "--no-themes", "--no-mcp", "--no-approve"]

seedPiConfig :: FilePath -> IO ()
seedPiConfig piHome = do
    srcDir <- piAgentConfigDir
    let dstDir = piHome </> ".pi" </> "agent"
    createDirectoryIfMissing True dstDir
    present <- filterM (doesFileExist . (srcDir </>)) ["models.json", "settings.json"]
    forM_ present $ \name -> copyFile (srcDir </> name) (dstDir </> name)

data SandboxPaths = SandboxPaths
    { sandboxWorktree :: FilePath
    , sandboxHome :: FilePath
    , sandboxPiHome :: FilePath
    , sandboxSessionId :: Text
    }

sessionPaths :: AgentSession -> SandboxPaths
sessionPaths session_ =
    SandboxPaths
        { sandboxWorktree = worktreePath session_
        , sandboxHome = sessionRoot </> "home"
        , sandboxPiHome = sessionRoot </> "pi"
        , sandboxSessionId = sessionId session_
        }
  where
    sessionRoot = takeDirectory (worktreePath session_)

expandSandboxArg :: SandboxPaths -> Text -> String
expandSandboxArg paths = T.unpack . flip (foldr (uncurry T.replace)) placeholders
  where
    placeholders =
        [ ("{worktree}", T.pack (sandboxWorktree paths))
        , ("{home}", T.pack (sandboxHome paths))
        , ("{sessionRoot}", T.pack (takeDirectory (sandboxWorktree paths)))
        , ("{sessionId}", sandboxSessionId paths)
        ]

runnerEnvironment :: [(String, String)] -> IO [(String, String)]
runnerEnvironment ownVars = chatApiKeyEnv >>= sandboxEnvironment ownVars

toolEnvironment :: [(String, String)] -> IO [(String, String)]
toolEnvironment ownVars = sandboxEnvironment ownVars Nothing

sandboxEnvironment :: [(String, String)] -> Maybe String -> IO [(String, String)]
sandboxEnvironment ownVars chatKeyEnv = do
    baseEnv <- getEnvironment
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

runnerConfigArgs :: (Text -> String) -> [Text] -> [String]
runnerConfigArgs expand = strip
  where
    strip [] = []
    strip (arg : rest)
        | arg `elem` managedWithValue = strip (drop 1 rest)
        | arg `elem` managedFlags = strip rest
        | otherwise = expand arg : strip rest
    managedWithValue = ["--mode", "--session", "--fork", "--model", "--thinking", "--provider"]
    managedFlags = ["-c", "--continue", "--no-session", "-p", "--print", "{prompt}"]

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

sandboxProcess :: AgentConfig -> [String] -> IO CreateProcess
sandboxProcess cfg args = do
    memoryMax <- agentMemoryMax
    case memoryMax of
        Nothing -> return (proc (agentSboxCommand cfg) args)
        Just _ -> do
            cgroup <- agentCgroup
            return (proc "bash" (["-c", "echo $$ > \"$0/cgroup.procs\" && exec \"$@\"", cgroup, agentSboxCommand cfg] ++ args))
