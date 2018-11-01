{-# LANGUAGE TemplateHaskell #-}

module Data.Secret
  ( Secret (..)
  , _Secret
  ) where

import Control.Lens (makePrisms)

newtype Secret a = Secret a
  deriving stock (Eq, Ord)

instance Show (Secret a) where
  showsPrec p _ = showParen (p > 10) $ showString "Secret _"

$(makePrisms ''Secret)
