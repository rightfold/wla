-- |
-- Crawling DSL.
module Wla.Crawl
  ( Crawl (..)
  , appendCookie
  , requestPage
  , crash
  , crashE
  ) where

import Control.Exception (Exception, SomeException (..))
import Control.Monad.Free.Class (MonadFree, liftF)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)

-- |
-- Crawling DSL.
data Crawl a
  = AppendCookie (() -> a) ByteString ByteString
  | RequestPage (Text -> a) ByteString Int ByteString
  | Crash SomeException
  deriving stock (Functor)

-- |
-- Set a cookie to be used by future requests.
appendCookie :: MonadFree Crawl m => ByteString -> ByteString -> m ()
appendCookie = (liftF .) . AppendCookie id

-- |
-- Request a page, returning its body.
requestPage :: MonadFree Crawl m => ByteString -> Int -> ByteString -> m Text
requestPage = ((liftF .) .) . RequestPage id

-- |
-- Crash the crawler; do not continue crawling.
crash :: (MonadFree Crawl m, Exception e) => e -> m a
crash = liftF . Crash . SomeException

-- |
-- 'either crash pure'.
crashE :: (MonadFree Crawl m, Exception e) => Either e a -> m a
crashE = either crash pure
