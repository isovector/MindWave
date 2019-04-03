module StreamingMindWave where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           MindWaveConnection
import           Streaming
import qualified Streaming.Prelude as S

mindWaveStream :: S.Stream (S.Of Readings) IO ()
mindWaveStream = do
  ref <- liftIO $ do
    ref <- newIORef initialMindWaveInfo
    void . forkIO $ readMind ref
    pure ref
  S.repeatM $ threadDelay 1000000
           *> fmap readings (readIORef ref)

