-- | Initial object.
module Data.Void
  ( Void
  , absurd
  ) where

-- | Initial object in Purs.
data Void

-- | Ex falso quodlibet.
foreign import absurd :: forall a. Void -> a
