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

class Discard a
instance discardUnit :: Discard Unit

discard :: forall a f b. Discard a => Bind f => f a -> (a -> f b) -> f b
discard = bind
