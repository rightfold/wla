-- |
-- Configuration.
module Wla.Config
  ( -- * Errors
    DecodeError

    -- * Configuration
  , Config (..)
  , readConfig
  , parseConfig
  , decodeConfig

    -- * Crawlers
  , Crawlers
  , Crawler (..)
  , readCrawlers
  , parseCrawlers
  , decodeCrawlers
  , decodeCrawler
  ) where

import Control.Exception (Exception, throwIO)
import Control.Lens ((^?), (%~), _Just, _Left, ix, to)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Lens as Ae
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bs.Lazy
import qualified Data.Scientific as Sci

import qualified Wla.Software.Zalando as Software.Zalando

--------------------------------------------------------------------------------
-- Errors

-- |
-- Cannot decode config.
data DecodeError
  = ParseError String
  | DecodeError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

--------------------------------------------------------------------------------
-- Configuration

-- |
-- Configuration.
data Config =
  Config
    { configHttpHost :: ByteString
    , configHttpPort :: Word16 }
  deriving stock (Eq, Show)

readConfig :: MonadIO m => FilePath -> m Config
readConfig = liftIO . (f >=> g)
  where f = fmap Bs.Lazy.fromStrict . Bs.readFile
        g = either throwIO pure . parseConfig

parseConfig :: Bs.Lazy.ByteString -> Either DecodeError Config
parseConfig = f . Ae.eitherDecode >=> g . decodeConfig
  where f = _Left %~ ParseError
        g = maybe (Left DecodeError) Right

decodeConfig :: Ae.Value -> Maybe Config
decodeConfig raw = do
  root     <- raw  ^? Ae._Object
  httpHost <- root ^? ix "httpHost" . Ae._String . to encodeUtf8
  httpPort <- root ^? ix "httpPort" . Ae._Number . to Sci.toBoundedInteger . _Just
  pure Config { configHttpHost = httpHost
              , configHttpPort = httpPort }

--------------------------------------------------------------------------------
-- Crawlers

-- |
-- Crawlers.
type Crawlers =
  [Crawler]

-- |
-- Crawler.
data Crawler
  = ZalandoCrawler Software.Zalando.Config
  deriving stock (Eq, Show)

readCrawlers :: MonadIO m => FilePath -> m Crawlers
readCrawlers = liftIO . (f >=> g)
  where f = fmap Bs.Lazy.fromStrict . Bs.readFile
        g = either throwIO pure . parseCrawlers

parseCrawlers :: Bs.Lazy.ByteString -> Either DecodeError Crawlers
parseCrawlers = f . Ae.eitherDecode >=> g . decodeCrawlers
  where f = _Left %~ ParseError
        g = maybe (Left DecodeError) Right

decodeCrawlers :: Ae.Value -> Maybe Crawlers
decodeCrawlers raw = do
  root <- raw ^? Ae._Array . to toList
  traverse decodeCrawler root

decodeCrawler :: Ae.Value -> Maybe Crawler
decodeCrawler raw = do
  root     <- raw ^? Ae._Object
  software <- root ^? ix "software" . Ae._String
  case software of
    "zalando" -> ZalandoCrawler <$> Software.Zalando.decodeConfig root
    _         -> Nothing
