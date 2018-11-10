-- | Wish list data types.
module Gui.WishList
  ( WishList
  , WishListItem (..)
  ) where

import Data.Maybe (Maybe)

-- | Wish lists are arrays instead of lists because they are never manipulated
-- | by the GUI; they are read-only.
type WishList =
  Array WishListItem

-- | Item on a wish list.
type WishListItem =
  { name     :: Maybe String
  , imageUrl :: Maybe String }
