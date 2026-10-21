module RollupCache (lookupRollupCache, withRollupCache, invalidateRollupCache) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TMVar, TVar, atomically, modifyTVar', newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, writeTVar)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

type RollupKey = (String, String, Int)

{-# NOINLINE rollupCacheRef #-}
rollupCacheRef :: MVar (Int, [(RollupKey, Value)])
rollupCacheRef = unsafePerformIO (newMVar (0, []))

rollupCacheLimit :: Int
rollupCacheLimit = 16

{-# NOINLINE rollupInFlightRef #-}
rollupInFlightRef :: TVar (Map RollupKey (TMVar (Either String Value)))
rollupInFlightRef = unsafePerformIO (newTVarIO Map.empty)

lookupRollupCache :: RollupKey -> IO (Maybe Value)
lookupRollupCache key = lookup key . snd <$> readMVar rollupCacheRef

currentRollupGeneration :: IO Int
currentRollupGeneration = fst <$> readMVar rollupCacheRef

insertRollupCache :: Int -> RollupKey -> Value -> IO ()
insertRollupCache generation key value =
    modifyMVar_ rollupCacheRef $ \(current, entries) ->
        return $
            if generation /= current
                then (current, entries)
                else (current, take rollupCacheLimit $ (key, value) : filter ((/= key) . fst) entries)

invalidateRollupCache :: IO ()
invalidateRollupCache = modifyMVar_ rollupCacheRef $ \(generation, _) -> return (generation + 1, [])

withRollupCache :: RollupKey -> IO (Either String Value) -> IO (Either String Value)
withRollupCache key compute = mask $ \restore -> do
    generation <- currentRollupGeneration
    role <- atomically $ do
        inFlight <- readTVar rollupInFlightRef
        case Map.lookup key inFlight of
            Just pending -> pure (Left pending)
            Nothing -> do
                pending <- newEmptyTMVar
                writeTVar rollupInFlightRef (Map.insert key pending inFlight)
                pure (Right pending)
    case role of
        Left pending -> atomically (readTMVar pending)
        Right pending -> do
            outcome <- try (restore compute)
            case outcome of
                Right result -> do
                    publish pending result
                    forM_ result (insertRollupCache generation key)
                    pure result
                Left err -> do
                    publish pending (Left (show (err :: SomeException)))
                    throwIO err
  where
    publish pending result =
        atomically $ do
            putTMVar pending result
            modifyTVar' rollupInFlightRef (Map.delete key)
