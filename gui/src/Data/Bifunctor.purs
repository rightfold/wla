module Data.Bifunctor
  ( class Bifunctor
  , lmap, rmap
  ) where

class Bifunctor f where
  lmap :: forall a b x. (a -> b) -> f a x -> f b x
  rmap :: forall a b x. (a -> b) -> f x a -> f x b
