-- |
-- Web application.
module Wla.Web
  ( application
  ) where

import Control.Monad.Trans.Class (lift)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http

import Wla.I18n (I18n)
import Wla.WishList (WishList)
import Wla.WishList.Html (renderWishList)

import qualified Network.Wai.Cont as Wai.Cont

-- |
-- Given a wish list acquisition action, return an application that uses the
-- action on each request.
application :: Monad m => I18n -> m WishList -> Wai.Cont.Application m
application i18n getWishList _req = do
  wishList <- lift getWishList
  pure . Wai.responseBuilder Http.status200 [("Content-Type", "text/html")] $
           renderHtmlBuilder (renderWishList i18n wishList)
