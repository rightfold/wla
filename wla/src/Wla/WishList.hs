-- |
-- Wish list data types.
module Wla.WishList
  ( WishList
  , WishListItem (..)
  ) where

import Data.Text (Text)

-- |
-- Wish list.
type WishList =
  [WishListItem]

-- |
-- Item on a wish list.
data WishListItem =
  WishListItem
    { itemName     :: Maybe Text
    , itemImageUrl :: Maybe Text }
  deriving stock (Eq, Show)
