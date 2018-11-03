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

    -- * Decoding
  , DecodeError (..)
  , decodeWishList
  , decodeWishListPage
  , decodeWishListData
  , decodeWishListData'
  ) where

import Control.Category ((>>>))
import Control.Exception (Exception)
import Control.Lens ((^.), (^?), (%~), _Left, ix)
import Control.Monad ((>=>))
import Control.Monad.Free.Class (MonadFree)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Lens as Ae
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bs.Lazy
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy

import Data.Secret (Secret, _Secret)
import Wla.Crawl (Crawl)
import Wla.WishList (WishList, WishListItem (..))

import qualified Wla.Crawl as Crawl

--------------------------------------------------------------------------------
-- Requesting

-- |
-- Zalando configuration.
data Config =
  Config
    { configHost  :: Bs.ByteString
    , configToken :: Secret Bs.ByteString }
  deriving stock (Eq, Show)

requestWishList :: MonadFree Crawl m => Config -> m WishList
requestWishList config = do
  wishListPage <- requestWishListPage config
  Crawl.crashE $ decodeWishList wishListPage

requestWishListPage :: MonadFree Crawl m => Config -> m Text.Lazy.Text
requestWishListPage config = do
  -- The cookies are nicely documented in the privacy policy. It seems we only
  -- need one to request the wish list page.
  Crawl.appendCookie "zac" (configToken config ^. _Secret)
  Crawl.requestPage (configHost config) 443 "/wishlist"

--------------------------------------------------------------------------------
-- Decoding

-- |
-- Something went wrong while decoding.
data DecodeError
  = CannotFindData        -- ^ Cannot find the wish list data.
  | JsonParseError String -- ^ Cannot parse the JSON data.
  | JsonDecodeError       -- ^ Cannot decode the parsed JSON data.
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
  where f = _Left %~ JsonParseError

-- |
-- Like 'decodeWishListData', but with JSON already parsed.
decodeWishListData' :: Ae.Value -> Either DecodeError WishList
decodeWishListData' = decodeRoot >>> maybe (Left JsonDecodeError) Right
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
