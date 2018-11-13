module Data.Either
  ( Either (..)
  , type (+)
  , either
  ) where

-- | Coproduct.
data Either a b
  = Left a
  | Right b
infixr 6 type Either as +

-- | Catamorphism.
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ f (Right x) = f x
