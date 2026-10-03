module RollupCache (lookupRollupCache, currentRollupGeneration, insertRollupCache, invalidateRollupCache) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (Value)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE rollupCacheRef #-}
rollupCacheRef :: MVar (Int, [((String, Int), Value)])
rollupCacheRef = unsafePerformIO (newMVar (0, []))

rollupCacheLimit :: Int
rollupCacheLimit = 16

lookupRollupCache :: String -> Int -> IO (Maybe Value)
lookupRollupCache commit projectId = lookup (commit, projectId) . snd <$> readMVar rollupCacheRef

currentRollupGeneration :: IO Int
currentRollupGeneration = fst <$> readMVar rollupCacheRef

insertRollupCache :: Int -> String -> Int -> Value -> IO ()
insertRollupCache generation commit projectId value =
    modifyMVar_ rollupCacheRef $ \(current, entries) ->
        return $
            if generation /= current
                then (current, entries)
                else (current, take rollupCacheLimit $ ((commit, projectId), value) : filter ((/= (commit, projectId)) . fst) entries)

invalidateRollupCache :: IO ()
invalidateRollupCache = modifyMVar_ rollupCacheRef $ \(generation, _) -> return (generation + 1, [])
