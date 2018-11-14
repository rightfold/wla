module Network.Ajax
  ( Request
  , Response
  , request'
  ) where

import Control.Semigroupoid (($))
import Control.Effect (Effect)
import Control.Monad.Error (attempt)
import Data.Either (type (+), (??))
import Data.Functor ((<$>), bind, discard)
import Data.Unit (Unit)
import Data.Void (Void)

import Dom as Dom

type Request =
  { method :: String
  , url    :: String }

type Response =
  { body :: String }

-- TODO: Add request which uses ContT.

request' :: Request -> (Dom.Error () + Response -> Effect Void Unit) -> Effect (Dom.Error ()) Unit
request' req cont = do
  xhr <- Dom.newXmlHttpRequest
  Dom.eventTargetAddEventListener xhr "load" \_ -> callback xhr
  Dom.xmlHttpRequestOpen xhr req.method req.url
  Dom.xmlHttpRequestSend xhr
  where
  callback :: forall a. Dom.XmlHttpRequest a -> Effect Void Unit
  callback xhr = do
    body <- attempt $
      -- TODO: If the body is not present, throw an exception instead of
      -- TODO: defaulting to the empty string.
      (_ ?? "") <$> Dom.xmlHttpRequestGetResponseText xhr
    cont $ { body: _ } <$> body
