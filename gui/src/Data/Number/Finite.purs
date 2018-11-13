module Data.Number.Finite
  ( Finite
  ) where

-- | Like Number, but never infinite or NaN.
newtype Finite =
  Finite Number

-- TODO: Add _Finite prism.
