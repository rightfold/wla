module Data.Functor
  ( class Functor
  , map
  , void
  ) where

import Data.Unit (Unit, unit)

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

void :: forall f a. Functor f => f a -> f Unit
void = map \_ -> unit
