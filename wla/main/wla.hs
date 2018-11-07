module Main
  ( main
  ) where

import Pipes ((>->), runEffect)
import System.Environment (getArgs)

import qualified Control.Concurrent.Async as Async
import qualified Data.IORef as IORef
import qualified Network.HTTP.Client.TLS as Http.Tls

import Wla.Config (Config (..), Crawler (..), readConfig, readCrawlers)
import Wla.Process.Crawl (crawlProcess)
import Wla.Process.Web (webProcess)

import qualified System.Posix.Signals.Extra as Sig

main :: IO ()
main = do
  -- Globals.
  http <- Http.Tls.newTlsManager
  sighups <- Sig.waiter Sig.Sighup
  (config, getCrawlers) <- getConfig
  wishListRef <- IORef.newIORef []

  -- Actors.
  crawlActor <- Async.async . runEffect $
                  sighups >-> crawlProcess http getCrawlers wishListRef
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
