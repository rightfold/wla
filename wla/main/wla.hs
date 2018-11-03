module Main
  ( main
  ) where

import Control.Monad.Free (foldFree)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Network.HTTP.Client.TLS as Http.Tls
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bs.Lazy

import Control.Logger (Logger (..))
import Data.Secret (Secret (..))
import Wla.Software.Zalando (Config (..), requestWishList)
import Wla.WishList.Html (renderWishList)

import qualified Wla.Crawl.Http as Crawl.Http
import qualified Wla.Crawl.Log as Crawl.Log
import qualified Wla.Crawl.UpstreamDyke as Crawl.UpstreamDyke

main :: IO ()
main = do
  tokenFile <- head <$> getArgs
  token <- Secret <$> Bs.readFile tokenFile

  http <- Http.Tls.newTlsManager

  let config = Config "zalando.nl" token
  wishList <- Crawl.Http.runT http $
    let interpret = Crawl.Log.interpret (Logger (liftIO . print)) $
                      Crawl.UpstreamDyke.interpret $
                        Crawl.Http.interpret in
    foldFree interpret $ requestWishList config

  Bs.Lazy.putStr (renderHtml (renderWishList wishList))
