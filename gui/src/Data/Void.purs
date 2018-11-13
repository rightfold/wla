module Data.Void
  ( Void
  , absurd
  ) where

data Void

foreign import absurd :: forall a. Void -> a
