module Control.Bind
  ( class Bind
  , class Discard
  , bind, (>>=)
  , discard
  ) where

import Data.Functor (class Functor)
import Data.Unit (Unit)

infixl 1 bind as >>=

class Functor f <= Bind f where
  bind :: forall a b. f a -> (a -> f b) -> f b

class Discard a where
  discard :: forall f b. Bind f => f a -> (a -> f b) -> f b

instance discardUnit :: Discard Unit where
  discard = bind
