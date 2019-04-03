{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.Foldable
import           Data.Time
import qualified Streaming.Prelude as S
import           StreamingMindWave


dumpHeader :: IO String
dumpHeader = do
  tz   <- getCurrentTimeZone
  time <- getCurrentTime
  pure $ formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz time)


main :: IO ()
main = S.mapM_ (\a -> do
         header <- dumpHeader
         B.appendFile "/home/sandy/brainwaves" $ B.pack $ header ++ show a ++ "\n"
       )
     . pluckEvery 15
     . S.map fold
     . S.slidingWindow 15
     $ mindWaveStream


pluckEvery :: Monad m => Int -> S.Stream (S.Of a) m r -> S.Stream (S.Of a) m r
pluckEvery n
  = S.map snd
  . S.filter ((== 0) . fst)
  . S.zip (S.cycle $ S.each [0 .. (n - 1)])

