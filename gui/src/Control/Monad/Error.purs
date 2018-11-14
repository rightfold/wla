module Control.Monad.Error
  ( class MonadThrow
  , class MonadError
  , class MonadBlunder
  , throw
  , catch
  , rid
  , attempt
  ) where

import Control.Semigroupoid ((<<))
import Data.Either (type (+), Either (..))
import Data.Functor (class Functor, class Monad, map, pure)

class Monad f <= MonadThrow e f | f -> e where
  throw :: forall a. e -> f a

class MonadThrow e f <= MonadError e f | f -> e where
  catch :: forall a. f a -> (e -> f a) -> f a

-- | Adapted from https://tinyurl.com/monadblunder (retrieved on November 13th,
-- | 2018).
class (MonadError e f, Monad g) <= MonadBlunder e g f | f -> e g where
  rid :: forall a. f a -> (e -> g a) -> g a

attempt :: forall e g f a. Functor f => MonadBlunder e g f => f a -> g (e + a)
attempt action = map Right action `rid` (pure << Left)
