-- |
-- Crawler activity logging.
module Wla.Crawl.Log
  ( Message (..)
  , interpret
  ) where

import Control.Exception (SomeException)
import Control.Logger (Logger, (<<))
import Data.ByteString (ByteString)
import Wla.Crawl (Crawl (..))

-- |
-- Log message.
data Message
  = RequestPageMessage ByteString Int ByteString
  | CrashMessage SomeException
  deriving stock (Show)

-- |
-- Interpreter.
interpret :: Applicative m => Logger m Message -> (Crawl a -> m b) -> Crawl a -> m b

interpret _ lower action@AppendCookie{} =
  lower action

interpret logger lower action@(RequestPage _ host port path) = do
  _ <- logger << RequestPageMessage host port path
  r <- lower action
  pure r

interpret logger lower action@(Crash e) = do
  _ <- logger << CrashMessage e
  r <- lower action
  pure r
