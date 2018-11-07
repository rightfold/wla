{-# LANGUAGE TemplateHaskell #-}

-- |
-- Extra utilities for http-client.
module Network.HTTP.Client.Extra
  ( -- * Optics
    _HttpExceptionRequest
  , _InvalidUrlException
  , requestHeaders

    -- * Security
  , redactingCookie
  , redactCookie
  ) where

import Control.Lens (Lens', (.~), _1, lens, makePrisms)
import Control.Monad.Catch (MonadCatch, handle, throwM)

import Network.HTTP.Client (HttpException, Request)
import Network.HTTP.Types (RequestHeaders)

import Control.Lens.Extra (ixAssoc)

import qualified Network.HTTP.Client as Http

--------------------------------------------------------------------------------
-- Optics

$(makePrisms ''HttpException)

requestHeaders :: Lens' Request RequestHeaders
requestHeaders = lens get set
  where get = Http.requestHeaders
        set r h = r { Http.requestHeaders = h }

--------------------------------------------------------------------------------
-- Security

-- |
-- Censor the Cookie header of the request in an exception.
redactingCookie :: MonadCatch m => m a -> m a
redactingCookie = handle (throwM . redactCookie)

-- |
-- Censor the Cookie header of the request in an exception.
redactCookie :: HttpException -> HttpException
redactCookie = _HttpExceptionRequest . _1 . requestHeaders . ixAssoc "Cookie" .~ "REDACTED"
