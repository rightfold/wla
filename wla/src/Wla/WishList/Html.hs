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

import Wla.I18n (I18n (..))
import Wla.WishList (WishList, WishListItem (..))

renderWishList :: I18n -> WishList -> Html
renderWishList i18n = E.ul . foldMap (E.li . renderWishListItem i18n)

renderWishListItem :: I18n -> WishListItem -> Html
renderWishListItem I18n{..} item =
  E.article $ do
    E.h1 . H.text $ fromMaybe i18nWishListItemNamePlaceholder (itemName item)
    E.img ! A.src (H.toValue (fromMaybe placeholderImageUrl (itemImageUrl item)))
    E.button . H.text $ i18nWishListItemViewInWebshop
    E.button . H.text $ i18nWishListItemShare

placeholderImageUrl :: Text
placeholderImageUrl = ""
