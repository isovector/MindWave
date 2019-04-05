{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable
import           Data.List.Utils (split)
import           Data.Time
import           GHC.Exts
import           GHC.Generics
import           MindWaveConnection (Readings)
import qualified Streaming.Prelude as S
import           StreamingMindWave


data Datapoint = Datapoint
  { date     :: String
  , moment   :: Int
  , readings :: Readings
  } deriving (Generic)

instance ToJSON Datapoint where
  toJSON (Datapoint d m r) =
    let Object r' = toJSON r
     in Object $ r' <> fromList [("date", toJSON d), ("moment", toJSON m)]


dumpHeader :: IO (String, Int)
dumpHeader = do
  tz   <- getCurrentTimeZone
  time <- getCurrentTime
  let day = formatTime defaultTimeLocale "%F" $ utcToLocalTime tz time
      ts = formatTime defaultTimeLocale "%X" $ utcToLocalTime tz time
      [h, m, _] = split ":" ts
  pure (day, read h * 60 + read m)


main :: IO ()
main = S.mapM_ (\a -> do
         (d, m) <- dumpHeader
         let file = "/home/sandy/.arbtt/brain/" <> d
         B.appendFile file $ toStrict $ encode $ Datapoint d m a
         B.appendFile file $ B.pack "\n"
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

