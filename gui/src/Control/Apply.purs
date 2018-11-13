module Control.Apply
  ( class Apply
  , apply, (<*>)
  , applyFirst, (<*)
  , applySecond, (*>)
  ) where

import Data.Functor (class Functor, (<$>))
import Data.Unit (Unit)

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
infixl 4 apply as <*>

applyFirst :: forall f a. Apply f => f a -> f Unit -> f a
applyFirst a b = (\x _ -> x) <$> a <*> b
infixl 4 applyFirst as <*

applySecond :: forall f a. Apply f => f Unit -> f a -> f a
applySecond a b = (\_ x -> x) <$> a <*> b
infixl 4 applySecond as *>
