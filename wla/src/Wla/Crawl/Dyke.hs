-- |
-- Flood protection. Crawlers should not make requests too rapidly.
module Wla.Crawl.Dyke
  ( interpret
  , delay
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Wla.Crawl (Crawl (..))

-- |
-- Interpreter.
interpret :: (MonadIO m) => (Crawl a -> m b) -> Crawl a -> m b
interpret lower action@AppendCookie{} = lower action
interpret lower action@RequestPage{}  = do { liftIO $ threadDelay delay
                                           ; lower action }
interpret lower action@Crash{}        = lower action

-- |
-- Delay before each request, in microseconds.
delay :: Integral a => a
delay = 1 * 1000 * 1000
