-- |
-- Handling signals.
module System.Posix.Signals.Extra
  ( Signal (..)
  , waiter
  ) where

import Data.Functor (void)

import qualified Control.Concurrent.Chan as Chan
import qualified System.Posix.Signals as Sig

-- |
-- Signal.
data Signal
  = Sighup
  deriving stock (Eq, Ord, Show)

-- |
-- Set up a signal handler and return an I/O action that blocks until the
-- signal was received.
waiter :: Signal -> IO (IO ())
waiter signal = do
  chan <- Chan.newChan
  let handler = void $ Chan.writeChan chan ()
  _ <- Sig.installHandler (reifySignal signal) (Sig.Catch handler) Nothing
  pure $ Chan.readChan chan

reifySignal :: Signal -> Sig.Signal
reifySignal Sighup = Sig.sigHUP
