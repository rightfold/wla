-- | Terminal object.
module Data.Unit
  ( Unit
  , unit
  ) where

-- | Terminal object in Purs.
foreign import data Unit :: Type

-- | Sole inhabitant of Unit.
foreign import unit :: Unit
