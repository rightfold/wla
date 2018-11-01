-- |
-- The Zalando software is proprietary, undocumented, and unstable. What is
-- documented here was found through experimentation and may be incomplete or
-- outdated.
--
-- The Zalando wish list page is rendered server-side. Embedded in the HTML
-- page is a data attribute that contains JSON data. This JSON data contains
-- the items on the wish list as well as some layout configuration and whether
-- the items are in stock.

module Wla.Software.Zalando
  ( -- * Requesting
    Config (..)
  , requestWishList
  , requestWishListPage
  , wishListPageRequest

    -- * Decoding
  , DecodeError (..)
  , decodeWishList
  , decodeWishListPage
  , decodeWishListData
  , decodeWishListData'
  ) where

import Control.Category ((>>>))
import Control.Exception (Exception, throwIO)
import Control.Lens ((^.), (^?), (%~), _Left, ix, to)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Lens as Ae
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bs.Lazy
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Network.HTTP.Client as Http

import Data.Secret (Secret, _Secret)
import Wla.WishList (WishList, WishListItem (..))

--------------------------------------------------------------------------------
-- Requesting

-- |
-- Zalando configuration.
data Config =
  Config
    { configHost  :: Bs.ByteString
    , configToken :: Secret Bs.ByteString }
  deriving stock (Eq, Show)

requestWishList :: MonadIO m => Http.Manager -> Config -> m WishList
requestWishList http config = do
  wishListPage <- requestWishListPage http config
  either (liftIO . throwIO) pure $
    decodeWishList wishListPage

requestWishListPage :: MonadIO m => Http.Manager -> Config -> m Text.Lazy.Text
requestWishListPage http config = do
  response <- liftIO $ Http.httpLbs (wishListPageRequest config) http
  pure . Text.Lazy.decodeUtf8With Text.lenientDecode $
           Http.responseBody response

-- |
-- HTTP request for Zalando wish list page.
wishListPageRequest :: Config -> Http.Request
wishListPageRequest config =
  Http.defaultRequest
    { Http.host   = configHost config
    , Http.port   = 443
    , Http.secure = True
    , Http.path   = "/wishlist"
    , Http.requestHeaders = [("Cookie", cookie)] }
  where
  -- The cookies are nicely documented in the privacy policy. It seems we only
  -- need one to request the wish list page.
  cookie :: Bs.ByteString
  cookie = configToken config ^. _Secret . to ("zac=" <>)

--------------------------------------------------------------------------------
-- Decoding

-- |
-- Something went wrong while decoding.
data DecodeError
  = CannotFindData         -- ^ Cannot find the wish list data.
  | JsonDecodeError String -- ^ Cannot decode the wish list data.
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- |
-- The full decoding pipeline.
decodeWishList :: Text.Lazy.Text -> Either DecodeError WishList
decodeWishList = decodeWishListPage >=> decodeWishListData

-- |
-- Decode the wish list page, which is a HTML page containing the wish list
-- data somewhere inside it. Returns the wish list data in JSON form.
decodeWishListPage :: Text.Lazy.Text -> Either DecodeError Bs.Lazy.ByteString
decodeWishListPage htmlPage =
  let
    -- The wish list data comes between these needles.
    dataStartNeedle = "data-props='"
    dataEndNeedle   = '\''

    -- Split HTML page on start needle, then strip needle.
    (_, onNeedle) = Text.Lazy.breakOn dataStartNeedle htmlPage
    afterNeedle = Text.Lazy.drop (Text.Lazy.length dataStartNeedle) onNeedle

    -- Find all data until the end needle.
    escapedData = Text.Lazy.takeWhile (/= dataEndNeedle) afterNeedle

    -- Decode HTML entities.
    unescapedData = Text.Lazy.replace "&quot;" "\"" escapedData
  in
    case unescapedData of
      "" -> Left CannotFindData
      ok -> Right (Text.Lazy.encodeUtf8 ok)

-- |
-- Decode the wish list data stored in the data attribute, which is in JSON
-- format. Returns the wish list extracted from the wish list data.
decodeWishListData :: Bs.Lazy.ByteString -> Either DecodeError WishList
decodeWishListData = f . Ae.eitherDecode' >=> decodeWishListData'
  where f = _Left %~ JsonDecodeError

-- |
-- Like 'decodeWishListData', but with JSON already parsed.
decodeWishListData' :: Ae.Value -> Either DecodeError WishList
decodeWishListData' = decodeRoot >>> maybe (Left (JsonDecodeError "")) Right
  where
  decodeRoot :: Ae.Value -> Maybe WishList
  decodeRoot raw = do
    root     <- raw  ^? Ae._Object
    articles <- root ^? ix "articleByConfigs" . Ae._Object
    traverse decodeArticle (HashMap.elems articles)

  decodeArticle :: Ae.Value -> Maybe WishListItem
  decodeArticle raw = do
    article      <- raw        ^? Ae._Object
    name         <- article    ^? ix "name" . Ae._String
    multimedia   <- article    ^? ix "multimedia" . Ae._Array
    let imageUrl =  multimedia ^? ix 0 . Ae._Object . ix "large_url" . Ae._String
    pure WishListItem { itemName     = Just name
                      , itemImageUrl = imageUrl }
