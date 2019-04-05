{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

module Average where

import Control.Arrow
import Data.Aeson
import GHC.Generics
import Generics.OneLiner


class Num a => HasDiv a where
  the_div :: a -> a -> a

instance HasDiv Int where
  the_div = div

instance HasDiv Float where
  the_div = (/)

data Average a = Average a Int
  deriving (Functor, Generic)

instance Read a => Read (Average a) where
  readsPrec i s = fmap (first toAverage) $ readsPrec i s

instance Show (Average Float) where
  show = show . getAverage

instance Show (Average Int) where
  show = show . getAverage

instance Num a => Semigroup (Average a) where
  (<>) = binaryOp @Num (+)

instance Num a => Monoid (Average a) where
  mempty = Average 0 0

instance ToJSON (Average Float) where
  toJSON = toJSON . getAverage

instance ToJSON (Average Int) where
  toJSON = toJSON . getAverage


toAverage :: a -> Average a
toAverage n = Average n 1


getAverage :: HasDiv a => Average a -> a
getAverage (Average n d) = the_div n $ fromIntegral d


