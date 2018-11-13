module Data.Profunctor
  ( class Profunctor
  , class Choice
  , lpmap
  , rpmap
  , left
  , right
  ) where

import Data.Either (type (+))

class Profunctor p where
  lpmap :: forall a b x. (b -> a) -> p a x -> p b x
  rpmap :: forall a b x. (a -> b) -> p x a -> p x b

class Profunctor p <= Choice p where
  left  :: forall a b x. p a b -> p (a + x) (b + x)
  right :: forall a b x. p a b -> p (x + a) (x + b)
