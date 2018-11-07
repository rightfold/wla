-- |
-- Composable loggers.
module Control.Logger
  ( Logger (..)
  , (<<)
  , sieved
  ) where

import Control.Monad.Morph (MFunctor, hoist)
import Data.Functor.Apply (Apply, (.>))
import Data.Functor.Contravariant (Contravariant, (>$<), contramap)
import Data.Functor.Contravariant.Divisible (Decidable, Divisible, choose, conquer, divide, lose)
import Data.Profunctor.Sieve (Sieve, sieve)
import Data.Semigroup (Semigroup, (<>))
import Data.Void (absurd)

infixr 0 <<

-- |
-- Logger.
newtype Logger f a =
  Logger { emit :: a -> f () }

-- |
-- Infix alias for 'emit'. The precedence and fixity allow it to be used with
-- '$' on the right-hand side: @logger << fst $ element@ is equivalent to
-- @emit logger (fst element)@.
(<<) :: Logger f a -> a -> f ()
(<<) = emit
{-# INLINE (<<) #-}

-- |
-- Create a logger from an arrow.
sieved :: Sieve p f => p a () -> Logger f a
sieved = Logger . sieve

--------------------------------------------------------------------------------
-- Semigroup tower

instance Apply f => Semigroup (Logger f a) where
  (<>) l m = Logger ((.>) <$> emit l <*> emit m)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Logger f a) where
  mappend l m = Logger ((*>) <$> emit l <*> emit m)
  mempty = Logger (const (pure ()))
  {-# INLINE mappend #-}
  {-# INLINE mempty #-}

--------------------------------------------------------------------------------
-- Contravariant tower

instance Contravariant (Logger f) where
  contramap f l = Logger (emit l . f)
  {-# INLINE contramap #-}

instance Applicative f => Divisible (Logger f) where
  divide f l m = (fst . f >$< l) `mappend` (snd . f >$< m)
  conquer = mempty
  {-# INLINE divide #-}
  {-# INLINE conquer #-}

instance Applicative f => Decidable (Logger f) where
  choose f l m = Logger (either (emit l) (emit m) . f)
  lose f = Logger (absurd . f)
  {-# INLINE choose #-}
  {-# INLINE lose #-}

--------------------------------------------------------------------------------
-- MFunctor tower

instance MFunctor Logger where
  hoist f l = Logger (f . emit l)
