-- |
-- The crawl process is a thread that crawls when receiving SIGHUP.
module Wla.Process.Crawl
  ( crawlProcess
  ) where

import Control.Monad (forever)
import Control.Monad.Free (foldFree)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (fold)
import Data.IORef (IORef)
import Pipes (Consumer, await)

import qualified Data.IORef as IORef
import qualified Network.HTTP.Client as Http

import Control.Logger (Logger (..))
import Wla.Config (Crawler (..))
import Wla.WishList (WishList)

import qualified Wla.Crawl.Http as Crawl.Http
import qualified Wla.Crawl.Log as Crawl.Log
import qualified Wla.Crawl.UpstreamDyke as Crawl.UpstreamDyke
import qualified Wla.Software.Zalando as Software.Zalando

-- |
-- The process that retrieves wish lists. This is a pipe that typically
-- consumes SIGHUPs.
crawlProcess :: MonadIO m => Http.Manager -> m [Crawler] -> IORef WishList -> Consumer () m a
crawlProcess http getCrawlers wishListRef =
  forever $ do
    wishLists <- lift $ traverse (materializeCrawler http) =<< getCrawlers
    liftIO $ IORef.atomicWriteIORef wishListRef (fold wishLists)
    await

-- |
-- Construct an I/O action that retrieves a wish list, given a crawler.
materializeCrawler :: MonadIO m => Http.Manager -> Crawler -> m WishList
materializeCrawler http (ZalandoCrawler config) =
  Crawl.Http.runT http $
    let interpret = Crawl.Log.interpret (Logger (liftIO . print)) $
                      Crawl.UpstreamDyke.interpret $
                        Crawl.Http.interpret in
    foldFree interpret $ Software.Zalando.requestWishList config
