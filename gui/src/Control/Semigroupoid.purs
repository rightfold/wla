module Control.Semigroupoid
  ( class Semigroupoid
  , compose, (<<)
  ) where

class Semigroupoid p where
  compose :: forall a b c. p b c -> p a b -> p a c
infixr 9 compose as <<

instance semigroupoidFunction :: Semigroupoid Function where
  compose f g a = f (g a)
