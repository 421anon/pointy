module Bisect (bisectBatch) where

bisectBatch :: (Monad m, Monoid r) => ([a] -> m (Either e r)) -> (e -> a -> m r) -> [a] -> m r
bisectBatch _ _ [] = pure mempty
bisectBatch tryBatch onSingleFailure items = do
    outcome <- tryBatch items
    case (outcome, items) of
        (Right result, _) -> pure result
        (Left err, [single]) -> onSingleFailure err single
        (Left _, _) -> do
            let (left, right) = splitAt (length items `div` 2) items
            (<>) <$> bisectBatch tryBatch onSingleFailure left <*> bisectBatch tryBatch onSingleFailure right
