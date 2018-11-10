module Control.Applicative
  ( class Applicative
  , pure
  ) where

import Control.Apply (class Apply)

class Apply f <= Applicative f where
  pure :: forall a. a -> f a
