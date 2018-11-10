module Control.Monad
  ( class Monad
  ) where

import Control.Applicative (class Applicative)
import Control.Bind (class Bind)

class (Applicative f, Bind f) <= Monad f
