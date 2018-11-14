module Data.Either
  ( Either (..), type (+)
  , either

  , Maybe
  , maybe
  , fromMaybe
  , coalesceMaybe, (??)
  ) where

import Data.Unit (Unit)

--------------------------------------------------------------------------------

-- | Coproduct.
data Either a b
  = Left a
  | Right b
infixr 6 type Either as +

-- | Catamorphism.
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ f (Right x) = f x

--------------------------------------------------------------------------------

-- | Successor.
type Maybe = Either Unit

-- | Catamorphism.
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe n _ (Left _)  = n
maybe _ j (Right x) = j x

-- | Collapse.
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe n (Left _)  = n
fromMaybe _ (Right x) = x

-- | Collapse.
coalesceMaybe :: forall a. Maybe a -> a -> a
coalesceMaybe (Left _)  n = n
coalesceMaybe (Right x) _ = x
infixl 2 coalesceMaybe as ??
