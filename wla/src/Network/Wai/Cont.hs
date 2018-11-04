-- |
-- Wai applications, but using 'ContT'.
module Network.Wai.Cont
  ( Application
  , RespondingT
  , downgrade
  ) where

import Control.Monad.Trans.Cont (ContT (..), runContT)

import qualified Network.Wai as Wai

-- |
-- Like 'Wai.Application', but based on 'ContT'.
type Application m = Wai.Request -> RespondingT m Wai.Response

-- |
-- Monad transformer.
type RespondingT = ContT Wai.ResponseReceived

-- |
-- Turn 'Application' into 'Wai.Application'.
downgrade :: Application m -> Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived
downgrade = (runContT .)
{-# INLINE downgrade #-}
