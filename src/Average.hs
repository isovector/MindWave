{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

module Average where

import GHC.Generics
import Generics.OneLiner

data Average a = Average a Int
  deriving (Functor, Generic)

instance Show (Average Float) where
  show = show . getAverage (/)

instance Show (Average Int) where
  show = show . getAverage div

instance Num a => Semigroup (Average a) where
  (<>) = binaryOp @Num (+)

instance Num a => Monoid (Average a) where
  mempty = Average 0 0

toAverage :: Num a => a -> Average a
toAverage n = Average n 1


getAverage :: Num a => (a -> a -> a) -> Average a -> a
getAverage divide (Average n d) = divide n $ fromIntegral d


