-- |
-- Handling signals.
module Wla.Infra.Signals
  ( Sighups
  , sighups
  , awaitSighup
  ) where

import Data.Functor (void)

import qualified Control.Concurrent.MVar as MVar
import qualified System.Posix.Signals as Sig

-- |
-- Action that awaits a SIGHUP signal.
newtype Sighups =
  Sighups (IO ())

-- |
-- Set up a SIGHUP signal handler and return an I/O action that blocks until
-- SIGHUP was received.
sighups :: IO Sighups
sighups = do
  signals <- MVar.newMVar ()
  let handler = void $ MVar.tryPutMVar signals ()
  _ <- Sig.installHandler Sig.sigHUP (Sig.Catch handler) Nothing
  pure . Sighups $ MVar.takeMVar signals

-- |
-- Await SIGHUP.
awaitSighup :: Sighups -> IO ()
awaitSighup (Sighups a) = a
