{-# LANGUAGE BlockArguments #-}

module Main where

import           Data.Foldable
import           Data.Time
import           Data.Time.LocalTime
import qualified Streaming.Prelude as S
import           StreamingMindWave
import           Text.Printf


dumpHeader :: IO ()
dumpHeader = do
  tz <- getCurrentTimeZone
  time <- getCurrentTime
  putStrLn $ formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz time)


main :: IO ()
main = S.mapM_ (\a -> do
         dumpHeader
         print a
         putStrLn ""
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

