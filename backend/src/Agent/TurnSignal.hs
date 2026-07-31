{- | Wakeup channels for turn log streams.

Writers signal after appending to a turn log (or saving final turn
state) so connected streams can block on STM instead of polling the
log file on a timer.  The log file remains the source of truth; the
channels only carry wakeups.
-}
module Agent.TurnSignal (
    registerTurnSignal,
    unregisterTurnSignal,
    signalTurnLog,
) where

import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTChan, newTVarIO, readTVar, writeTChan, writeTVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath (normalise)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE signals #-}
signals :: TVar (Map FilePath (TChan ()))
signals = unsafePerformIO $ newTVarIO Map.empty

{- | Get (or create) the wakeup channel for a turn log.  Concurrent
streams for the same turn share one channel.
-}
registerTurnSignal :: FilePath -> IO (TChan ())
registerTurnSignal path = atomically $ do
    let key = normalise path
    m <- readTVar signals
    case Map.lookup key m of
        Just ch -> pure ch
        Nothing -> do
            ch <- newTChan
            writeTVar signals (Map.insert key ch m)
            pure ch

{- | Stop tracking a turn log.  Idempotent; safe to call when the turn
finishes even if no stream ever connected.
-}
unregisterTurnSignal :: FilePath -> IO ()
unregisterTurnSignal path =
    atomically $ modifyTVar' signals (Map.delete (normalise path))

{- | Wake every stream watching this turn log.  No-op when nobody is
watching.
-}
signalTurnLog :: FilePath -> IO ()
signalTurnLog path = atomically $ do
    m <- readTVar signals
    maybe (pure ()) (\ch -> writeTChan ch ()) (Map.lookup (normalise path) m)
