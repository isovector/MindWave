{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import           Average
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable
import           Data.List.Utils (split)
import           Data.Time
import           GHC.Exts
import           GHC.Generics
import           MindWaveConnection (Readings, hoist)
import qualified Streaming.Prelude as S
import           StreamingMindWave
import           System.Environment

type family Defun f a where
  Defun Average a = a
  Defun f a = f a

data Datapoint f = Datapoint
  { date     :: String
  , moment   :: Defun f Float
  , readings :: Readings f
  } deriving (Generic)

instance Semigroup (Datapoint []) where
  Datapoint d1 m1 r1 <> Datapoint _ m2 r2 = Datapoint d1 (m1 <> m2) (r1 <> r2)

deriving instance Show (Datapoint [])
deriving instance Show (Datapoint Average)
deriving instance Read (Datapoint Average)

jonk :: Datapoint Average -> Datapoint []
jonk = hoist @HasDiv (pure . getAverage)

columnizeDPs :: [Datapoint Average] -> Datapoint []
columnizeDPs [dp]       = jonk dp
columnizeDPs (dp : dps) = jonk dp <> columnizeDPs dps
columnizeDPs []         = error "bad boy"

instance ToJSON (Datapoint []) where
  toJSON (Datapoint d m r) =
    let Object r' = toJSON r
     in Object $ r' <> fromList [("date", toJSON d), ("moment", toJSON m)]


dumpHeader :: IO (String, Float)
dumpHeader = do
  tz   <- getCurrentTimeZone
  time <- getCurrentTime
  let day = formatTime defaultTimeLocale "%F" $ utcToLocalTime tz time
      ts = formatTime defaultTimeLocale "%X" $ utcToLocalTime tz time
      [h, m, s] = split ":" ts
  pure (day, read h * 60 + read m + read s / 60)


freq :: Int
freq = 15

main :: IO ()
main = do
  args <- getArgs
  case args of
    [d] -> do
      z <- readFile $ "/home/sandy/.arbtt/brain/" <> d
      B.putStrLn $ toStrict $ encode $ columnizeDPs $ fmap read $ lines z

    [] -> do
      S.mapM_ (\a -> do
             (d, m) <- dumpHeader
             let file = "/home/sandy/.arbtt/brain/" <> d
             B.appendFile file $ B.pack $ show $ Datapoint d m a
             -- B.appendFile file $ toStrict $ encode $ Datapoint d m a
             B.appendFile file $ B.pack "\n"
           )
         . pluckEvery freq
         . S.map fold
         . S.slidingWindow freq
         $ mindWaveStream


pluckEvery :: Monad m => Int -> S.Stream (S.Of a) m r -> S.Stream (S.Of a) m r
pluckEvery n
  = S.map snd
  . S.filter ((== 0) . fst)
  . S.zip (S.cycle $ S.each [0 .. (n - 1)])

