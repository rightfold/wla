module Gui.I18n
  ( I18n
  , nlNl
  , enUs
  ) where

type I18n =
  { i18nWishListItemNamePlaceholder :: String
  , i18nWishListItemShare :: String
  , i18nWishListItemViewInWebshop :: String }

nlNl :: I18n
nlNl =
  { i18nWishListItemNamePlaceholder: "Onbekend"
  , i18nWishListItemShare: "Deel"
  , i18nWishListItemViewInWebshop: "Bekijk in webwinkel" }

enUs :: I18n
enUs =
  { i18nWishListItemNamePlaceholder: "Unknown"
  , i18nWishListItemShare: "Share"
  , i18nWishListItemViewInWebshop: "View in Webshop" }
