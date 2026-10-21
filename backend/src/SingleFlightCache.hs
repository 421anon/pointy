{-# LANGUAGE LambdaCase #-}

module SingleFlightCache (SingleFlightCache, newSingleFlightCache, cachedSuccess) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)

data SingleFlightCache k v = SingleFlightCache Int (MVar [(k, MVar (Maybe v))])

newSingleFlightCache :: Int -> IO (SingleFlightCache k v)
newSingleFlightCache limit = SingleFlightCache limit <$> newMVar []

cachedSuccess :: (Eq k) => SingleFlightCache k v -> k -> IO (Either e v) -> IO (Either e v)
cachedSuccess (SingleFlightCache limit entriesRef) key compute = do
    slot <- modifyMVar entriesRef $ \entries -> do
        slot <- maybe (newMVar Nothing) pure (lookup key entries)
        pure (take limit ((key, slot) : filter ((/= key) . fst) entries), slot)
    modifyMVar slot $ \case
        Just value -> pure (Just value, Right value)
        Nothing -> do
            outcome <- compute
            pure (either (const Nothing) Just outcome, outcome)
