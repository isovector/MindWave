{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad
import Data.IORef
import Control.Concurrent
import Control.Exception
import MindWaveConnection

-- | An example usage of the MindWaveConnection module, that creates an IORef,
--   forks off a thread for reading from the MindWave, and then uses this thread to
--   generate "images" representing the continaully updating MindWaveInfo
main :: IO ()
main = do
  ref <- newIORef initialMindWaveInfo
  mindWaveThread <- forkIO $ readMind ref
  finally
    do
      forever $ do
        mw <- readIORef ref
        print mw
        threadDelay 1000000
    do
      killThread mindWaveThread
      disconnect
      print "End of Program"
