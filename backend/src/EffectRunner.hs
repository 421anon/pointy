{-# LANGUAGE RankNTypes #-}

module EffectRunner (Runner (..), installRunner, runAppEffects) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Effectful (Eff)
import Effects (AppEffects)
import System.IO.Unsafe (unsafePerformIO)

newtype Runner = Runner {runWith :: forall x. Eff AppEffects x -> IO x}

{-# NOINLINE runnerRef #-}
runnerRef :: IORef Runner
runnerRef = unsafePerformIO (newIORef (Runner (\_ -> error "effect runner not installed")))

installRunner :: Runner -> IO ()
installRunner = writeIORef runnerRef

runAppEffects :: Eff AppEffects a -> IO a
runAppEffects action = do
    runner <- readIORef runnerRef
    runWith runner action
