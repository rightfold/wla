module Main
  ( main
  ) where

import Control.Monad (forever)
import Control.Monad.Free (foldFree)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor (void)
import Data.IORef (IORef)
import System.Environment (getArgs)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http.Tls
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Posix.Signals as Sig

import Control.Logger (Logger (..))
import Wla.Config (Config (..), Crawler (..), readConfig, readCrawlers)
import Wla.Software.Zalando (requestWishList)
import Wla.WishList (WishList)

import qualified Network.Wai.Cont as Wai.Cont
import qualified Wla.Crawl.Http as Crawl.Http
import qualified Wla.Crawl.Log as Crawl.Log
import qualified Wla.Crawl.UpstreamDyke as Crawl.UpstreamDyke
import qualified Wla.I18n as I18n
import qualified Wla.Web as Web

main :: IO ()
main = do
  -- Globals.
  http <- Http.Tls.newTlsManager
  sighups <- getSighups
  (config, getCrawlers) <- getConfig
  wishListRef <- IORef.newIORef []

  -- Actors.
  crawlActor <- Async.async $ crawlProcess http sighups getCrawlers wishListRef
  webActor   <- Async.async $ webProcess config wishListRef
  _ <- Async.waitAnyCancel [crawlActor, webActor]

  -- Should not happen.
  fail "Something went wrong"

-- |
-- Get the configuration.
getConfig :: IO (Config, IO [Crawler])
getConfig = do
  arguments <- getArgs
  (configPath, crawlersPath) <-
    case arguments of { [a, b] -> pure (a, b)
                      ; _      -> fail "Usage: wla CONFIG CRAWLERS" }

  config <- readConfig configPath
  let crawlers = readCrawlers crawlersPath

  pure (config, crawlers)

-- |
-- The process that retrieves wish lists.
crawlProcess :: Http.Manager -> IO () -> IO [Crawler] -> IORef WishList -> IO a
crawlProcess http sighups getCrawlers wishListRef =
  forever $ do
    sighups
    wishLists <- traverse (materializeCrawler http) =<< getCrawlers
    IORef.atomicWriteIORef wishListRef (fold wishLists)

-- |
-- The process that serves HTTP requests.
webProcess :: Config -> IORef WishList -> IO ()
webProcess config wishListRef =
  -- FIXME: Listen on configHttpHost, not the Warp default.
  Warp.run (configHttpPort config & fromIntegral) $
    Wai.Cont.downgrade $
      Web.application I18n.nlNl (IORef.readIORef wishListRef)

-- |
-- SIGHUP signals.
getSighups :: IO (IO ())
getSighups = do
  signals <- MVar.newMVar ()
  let handler = void $ MVar.tryPutMVar signals ()
  _ <- Sig.installHandler Sig.sigHUP (Sig.Catch handler) Nothing
  pure $ MVar.takeMVar signals

-- |
-- Construct an I/O action that retrieves a wish list, given a crawler.
materializeCrawler :: Http.Manager -> Crawler -> IO WishList
materializeCrawler http (ZalandoCrawler config) =
  Crawl.Http.runT http $
    let interpret = Crawl.Log.interpret (Logger (liftIO . print)) $
                      Crawl.UpstreamDyke.interpret $
                        Crawl.Http.interpret in
    foldFree interpret $ requestWishList config
