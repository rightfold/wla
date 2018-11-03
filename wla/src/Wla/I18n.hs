-- |
-- I18n.
module Wla.I18n
  ( I18n (..)
  , nlNl
  , enUs
  ) where

import Data.Text (Text)

data I18n =
  I18n
    { i18nWishListItemNamePlaceholder :: Text
    , i18nWishListItemShare :: Text
    , i18nWishListItemViewInWebshop :: Text }
  deriving (Eq, Show)

nlNl :: I18n
nlNl =
  I18n
    { i18nWishListItemNamePlaceholder = "Onbekend"
    , i18nWishListItemShare = "Deel"
    , i18nWishListItemViewInWebshop = "Bekijk in webwinkel" }

enUs :: I18n
enUs =
  I18n
    { i18nWishListItemNamePlaceholder = "Unknown"
    , i18nWishListItemShare = "Share"
    , i18nWishListItemViewInWebshop = "View in Webshop" }
