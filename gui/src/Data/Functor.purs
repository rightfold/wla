module Data.Functor
  ( class Functor
  , map, (<$>)
  , void
  ) where

import Control.Semigroupoid ((<<))
import Data.Unit (Unit, unit)

-- | Functor from Purs to a subcategory of Purs where f is applied to every
-- | object.
-- |
-- |  - forall xs. map id xs = xs
-- |  - forall f g. map f << map g = map (f << g)
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
infixl 4 map as <$>

instance functorFunction :: Functor (Function i) where
  map = (<<)

-- | Map morphism to terminal object.
void :: forall f a. Functor f => f a -> f Unit
void = map \_ -> unit
