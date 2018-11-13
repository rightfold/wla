module Control.Monad.Error
  ( class MonadThrow
  , class MonadError
  , class MonadBlunder
  , throw
  , catch
  , rid
  , swallow
  ) where

import Control.Applicative (pure)
import Control.Monad (class Monad)
import Data.Unit (Unit, unit)

class Monad f <= MonadThrow e f | f -> e where
  throw :: forall a. e -> f a

class MonadThrow e f <= MonadError e f | f -> e where
  catch :: forall a. f a -> (e -> f a) -> f a

-- | Adapted from https://tinyurl.com/monadblunder (retrieved on November 13th,
-- | 2018).
class (MonadError e f, Monad g) <= MonadBlunder e g f | f -> e g where
  rid :: forall a. f a -> (e -> g a) -> g a

swallow :: forall e g f. MonadBlunder e g f => f Unit -> g Unit
swallow action = action `rid` \_ -> pure unit
