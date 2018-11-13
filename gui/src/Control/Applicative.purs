module Control.Applicative
  ( class Applicative
  , pure
  , pu
  , ppu
  ) where

import Control.Apply (class Apply)
import Data.Unit (Unit, unit)

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeFunction :: Applicative (Function i) where
  pure x = \_ -> x

-- | Short-hand for pure unit.
pu :: forall f. Applicative f => f Unit
pu = pure unit

-- | Short-hand for pure (pure unit), and the very common const (pure unit).
ppu :: forall f g. Applicative f => Applicative g => f (g Unit)
ppu = pure pu
