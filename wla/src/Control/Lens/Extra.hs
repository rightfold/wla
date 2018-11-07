-- |
-- Lens utilities.
module Control.Lens.Extra
  ( ixAssoc
  ) where

import Control.Lens (Traversal', _2, each, filtered)

-- |
-- Index into an association list.
ixAssoc :: Eq a => a -> Traversal' [(a, b)] b
ixAssoc k = each . filtered ((==) k . fst) . _2
