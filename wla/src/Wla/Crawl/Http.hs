{-# LANGUAGE TemplateHaskell #-}

-- |
-- Crawl interpreter that performs HTTP requests.
module Wla.Crawl.Http
  ( Env (..)
  , State (..)
  , runT
  , interpret
  ) where

import Control.Exception (throwIO)
import Control.Lens ((%=), makeLenses, use, view)
import Control.Monad.Trans.RWS (RWST, evalRWST)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))

import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Network.HTTP.Client as Http

import Wla.Crawl (Crawl (..))

import qualified Network.HTTP.Client.Extra as Http.Extra

-- |
-- Interpreter environment.
data Env =
  Env
    { _envHttp :: Http.Manager }

-- |
-- Interpreter state.
data State =
  State
    { _stateCookie :: ByteString }
  deriving stock (Eq, Show)

$(makeLenses ''Env)
$(makeLenses ''State)

-- |
-- Run a monad transformer for interpretation.
runT :: (MonadIO m) => Http.Manager -> RWST Env () State m a -> m a
runT http action = fst <$> evalRWST action env state
  where env = Env { _envHttp = http }
        state = State { _stateCookie = "" }

-- |
-- Interpreter.
interpret
  :: ( MonadIO m
     , MonadReader Env m
     , MonadState State m )
  => Crawl a
  -> m a

interpret (AppendCookie next key value) = do
  stateCookie %= appendCookie key value
  pure $ next ()

interpret (RequestPage next host port path) = do
  http <- view envHttp
  request <- makeRequest host port path
  response <- liftIO . Http.Extra.redactingCookie $ Http.httpLbs request http
  pure . next . Text.Lazy.decodeUtf8With Text.lenientDecode $
                  Http.responseBody response

interpret (Crash exception) =
  liftIO $ throwIO exception

-- |
-- Make a request.
makeRequest
  :: (MonadState State m)
  => ByteString -- ^ Host
  -> Int        -- ^ Port
  -> ByteString -- ^ Path
  -> m Http.Request
makeRequest host port path = do
  cookie <- use stateCookie
  pure Http.defaultRequest
         { Http.host   = host
         , Http.port   = port
         , Http.secure = port == 443
         , Http.path   = path
         , Http.requestHeaders = [("Cookie", cookie)] }

-- |
-- Append a cookie.
appendCookie :: ByteString -> ByteString -> ByteString -> ByteString
appendCookie k v "" = k <> "=" <> v
appendCookie k v o  = o <> ";" <> k <> "=" <> v
