module Control.Monad.Error
  ( class MonadThrow
  , throw
  ) where

import Control.Monad (class Monad)

class Monad f <= MonadThrow e f | f -> e where
  throw :: forall a. e -> f a
