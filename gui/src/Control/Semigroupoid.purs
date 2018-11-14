module Control.Semigroupoid
  ( class Semigroupoid
  , compose, (<<)

  , class Category
  , id, ($)
  ) where

class Semigroupoid p where
  compose :: forall a b c. p b c -> p a b -> p a c
infixr 9 compose as <<

instance semigroupoidFunction :: Semigroupoid Function where
  compose f g a = f (g a)

class Category p where
  id :: forall a. p a a
infixr 0 id as $

instance categoryFunction :: Category Function where
  id a = a
