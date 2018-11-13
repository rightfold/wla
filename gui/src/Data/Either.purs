module Data.Either
  ( Either (..)
  , type (+)
  ) where

data Either a b
  = Left a
  | Right b
infixr 6 type Either as +
