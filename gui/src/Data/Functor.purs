module Data.Functor
  ( class Functor
  , map, (<$>)
  , void
  ) where

import Control.Semigroupoid ((<<))
import Data.Unit (Unit, unit)

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
infixl 4 map as <$>

instance functorFunction :: Functor (Function i) where
  map = (<<)

void :: forall f a. Functor f => f a -> f Unit
void = map \_ -> unit
