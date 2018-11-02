module Main
  ( main
  ) where

import Control.Monad.Free (foldFree)
import System.Environment (getArgs)

import qualified Network.HTTP.Client.TLS as Http.Tls
import qualified Data.ByteString as Bs

import Data.Secret (Secret (..))
import Wla.Software.Zalando (Config (..), requestWishList)

import qualified Wla.Crawl.Dyke as Crawl.Dyke
import qualified Wla.Crawl.Http as Crawl.Http

main :: IO ()
main = do
  tokenFile <- head <$> getArgs
  token <- Secret <$> Bs.readFile tokenFile

  http <- Http.Tls.newTlsManager

  let config = Config "zalando.nl" token
  wishList <- Crawl.Http.runT http $
    foldFree (Crawl.Dyke.interpret Crawl.Http.interpret) $
      requestWishList config

  print wishList
