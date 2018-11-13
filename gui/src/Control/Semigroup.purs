module Data.Semigroup
  ( class Semigroup
  , append, (<>)
  ) where

class Semigroup a where
  append :: a -> a -> a

instance semigroupString :: Semigroup String where
  append = appendS

foreign import appendS :: String -> String -> String
infixr 5 append as <>
