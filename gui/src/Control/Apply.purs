module Control.Apply
  ( class Apply
  , apply
  ) where

import Data.Functor (class Functor)

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
