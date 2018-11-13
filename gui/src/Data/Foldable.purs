module Data.Foldable
  ( class Foldable
  , foldl
  , foldr
  , for_
  ) where

import Control.Applicative (class Applicative, pure)
import Control.Apply (apply, (*>))
import Data.Functor (map)
import Data.Unit (Unit, unit)

class Foldable f where
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

instance foldableArray :: Foldable Array where
  foldl = foldlA
  foldr = foldrA

foreign import foldlA :: forall a b. (b -> a -> b) -> b -> Array a -> b
foreign import foldrA :: forall a b. (a -> b -> b) -> b -> Array a -> b

for_ :: forall f g a. Foldable f => Applicative g => f a -> (a -> g Unit) -> g Unit
for_ xs k = foldr (\a b -> k a *> b) (pure unit) xs
