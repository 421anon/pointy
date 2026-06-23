module Agent.Sandbox (
    nixDaemonBindArgs,
) where

import System.Directory (doesFileExist)

nixCompatSocket :: FilePath
nixCompatSocket = "/run/nix-daemon-socket"

nixDaemonBindArgs :: IO [String]
nixDaemonBindArgs = do
    exists <- doesFileExist nixCompatSocket
    return $
        if exists
            then ["--bind", "/run/nix-daemon-socket", "/var/run/nix-daemon-socket"]
            else []
