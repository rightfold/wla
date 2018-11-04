-- |
-- Web application.
module Wla.Web
  ( application
  ) where

import Control.Monad.Trans.Cont (ContT (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http

import Wla.I18n (I18n)
import Wla.WishList (WishList)
import Wla.WishList.Html (renderWishList)

-- |
-- Given a wish list acquisition action, return an application that uses the
-- action on each request.
application :: Monad m => I18n -> m WishList -> Wai.Request
            -> ContT Wai.ResponseReceived m Wai.Response
application i18n getWishList _req = ContT $ \res -> do
  wishList <- getWishList
  res . Wai.responseBuilder Http.status200 [("Content-Type", "text/html")] $
    renderHtmlBuilder (renderWishList i18n wishList)
