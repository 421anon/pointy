{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.IngestStream (ingestStreamHandler) where

import Control.Concurrent.STM (TChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Effects (AppM)
import IngestBus (IngestJob (..), snapshotAndSubscribe)
import Servant (Header, Headers, addHeader)
import qualified Servant.Types.SourceT as S
import qualified Sse

ingestStreamHandler ::
    AppM
        ( Headers
            '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text]
            (S.SourceT IO BS.ByteString)
        )
ingestStreamHandler = do
    (initialSnapshot, busChan) <- liftIO snapshotAndSubscribe
    let source =
            S.fromStepT
                ( S.Yield
                    (Sse.sseEvent "ingest-jobs" (encodeJobs initialSnapshot))
                    (S.Effect (streamLoop busChan))
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

streamLoop :: TChan (Map Int IngestJob) -> IO (S.StepT IO BS.ByteString)
streamLoop = Sse.broadcastLoop ((,) "ingest-jobs" . encodeJobs)

encodeJobs :: Map Int IngestJob -> LBS.ByteString
encodeJobs jobs = encode (object ["jobs" .= sortOn jobId (Map.elems jobs)])
