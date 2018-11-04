module Main
  ( main
  ) where

import Control.Monad.Free (foldFree)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Function ((&))
import System.Environment (getArgs)

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http.Tls
import qualified Network.Wai.Handler.Warp as Warp

import Control.Logger (Logger (..))
import Wla.Config (Config (..), Crawler (..), readConfig, readCrawlers)
import Wla.Software.Zalando (requestWishList)
import Wla.WishList (WishList)

import qualified Wla.Crawl.Http as Crawl.Http
import qualified Wla.Crawl.Log as Crawl.Log
import qualified Wla.Crawl.UpstreamDyke as Crawl.UpstreamDyke
import qualified Wla.I18n as I18n
import qualified Wla.Web as Web

main :: IO ()
main = do
  arguments <- getArgs
  (configPath, crawlersPath) <-
    case arguments of { [a, b] -> pure (a, b)
                      ; _      -> fail "Usage: wla CONFIG CRAWLERS" }

  config <- readConfig configPath
  crawlers <- readCrawlers crawlersPath

  http <- Http.Tls.newTlsManager
  let crawlers' = fmap (materializeCrawler http) crawlers

  wishList <- fold <$> sequence crawlers'

  -- FIXME: Listen on configHttpHost, not the Warp default.
  Warp.run (configHttpPort config & fromIntegral) $
    Web.application I18n.nlNl wishList

materializeCrawler :: Http.Manager -> Crawler -> IO WishList
materializeCrawler http (ZalandoCrawler config) =
  Crawl.Http.runT http $
    let interpret = Crawl.Log.interpret (Logger (liftIO . print)) $
                      Crawl.UpstreamDyke.interpret $
                        Crawl.Http.interpret in
    foldFree interpret $ requestWishList config
