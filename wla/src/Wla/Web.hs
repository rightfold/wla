-- |
-- Web application.
module Wla.Web
  ( application
  ) where

import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http

import Wla.I18n (I18n)
import Wla.WishList (WishList)
import Wla.WishList.Html (renderWishList)

application :: I18n -> WishList -> Wai.Application
application i18n wishList _req res =
  res . Wai.responseBuilder Http.status200 [("Content-Type", "text/html")] $
    renderHtmlBuilder (renderWishList i18n wishList)
