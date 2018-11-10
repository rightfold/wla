module Gui.WishList.Dom
  ( renderWishList
  , renderWishListItem
  ) where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Effect (Effect)
import Data.Foldable (for_)
import Data.Maybe ((??))
import Dom (Document, Element, Error, nodeAppendChild, nodeSetTextContent, documentCreateElement)

import Gui.I18n (I18n)
import Gui.WishList (WishList, WishListItem)

renderWishList :: forall a. I18n -> Document a -> WishList -> Effect (Error ()) (Element ())
renderWishList i18n document wishList = do
  ul <- documentCreateElement document "ul"
  for_ wishList \wishListItem -> do
    li <- renderWishListItem i18n document wishListItem
    nodeAppendChild ul li
  pure ul

renderWishListItem :: forall a. I18n -> Document a -> WishListItem -> Effect (Error ()) (Element ())
renderWishListItem i18n document wishListItem = do
  li <- documentCreateElement document "li"

  article <- documentCreateElement document "article"
  nodeAppendChild li article

  h1 <- documentCreateElement document "h1"
  nodeAppendChild article h1
  nodeSetTextContent h1 (wishListItem.name ?? i18n.i18nWishListItemNamePlaceholder)

  pure li
