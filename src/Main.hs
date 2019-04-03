{-# LANGUAGE BlockArguments #-}

module Main where

import           StreamingMindWave
import qualified Streaming.Prelude as S

main :: IO ()
main = S.print mindWaveStream
