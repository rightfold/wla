-- |
-- Handling signals.
module System.Posix.Signals.Extra
  ( Signal (..)
  , waiter
  ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Pipes (Producer, yield)

import qualified Control.Concurrent.Chan as Chan
import qualified System.Posix.Signals as Sig

-- |
-- Signal.
data Signal
  = Sighup
  deriving stock (Eq, Ord, Show)

-- |
-- Set up a signal handler and return a producer that yields signals.
waiter :: (MonadIO m, MonadIO m') => Signal -> m (Producer () m' a)
waiter signal = liftIO $ do
  chan <- Chan.newChan
  let handler = void $ Chan.writeChan chan ()
  _ <- Sig.installHandler (reifySignal signal) (Sig.Catch handler) Nothing
  pure . forever $ yield =<< liftIO (Chan.readChan chan)

reifySignal :: Signal -> Sig.Signal
reifySignal Sighup = Sig.sigHUP
