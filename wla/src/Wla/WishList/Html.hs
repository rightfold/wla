-- |
-- Render wish list to HTML.
module Wla.WishList.Html
  ( renderWishList
  , renderWishListItem
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Blaze.Html5 (Html, (!))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as E
import qualified Text.Blaze.Html5.Attributes as A

import Wla.WishList (WishList, WishListItem (..))

renderWishList :: WishList -> Html
renderWishList = E.ul . foldMap (E.li . renderWishListItem)

renderWishListItem :: WishListItem -> Html
renderWishListItem item =
  E.article $ do
    E.h1 . H.text $ fromMaybe placeholderText (itemName item)
    E.img ! A.src (H.toValue (fromMaybe placeholderImageUrl (itemImageUrl item)))

placeholderText :: Text
placeholderText = "Unknown"

placeholderImageUrl :: Text
placeholderImageUrl = ""
