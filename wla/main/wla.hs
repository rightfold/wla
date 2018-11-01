module Main
  ( main
  ) where

import Control.Monad.Free (foldFree)
import System.Environment (getArgs)

import qualified Network.HTTP.Client.TLS as Http.Tls
import qualified Data.ByteString as Bs

import Data.Secret (Secret (..))
import Wla.Crawl.Http (interpret, runT)
import Wla.Software.Zalando (Config (..), requestWishList)

main :: IO ()
main = do
  tokenFile <- head <$> getArgs
  token <- Secret <$> Bs.readFile tokenFile

  http <- Http.Tls.newTlsManager

  let config = Config "zalando.nl" token
  wishList <- runT http $ foldFree interpret (requestWishList config)

  print wishList
