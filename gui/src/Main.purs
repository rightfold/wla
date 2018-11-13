module Main
  ( main
  ) where

import Control.Applicative (pure)
import Control.Bind ((>>=), bind, discard)
import Control.Semigroupoid ((<<))
import Control.Effect (Effect)
import Control.Monad.Error (rid, throw)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe (..), maybe)
import Data.Unit (Unit, unit)
import Gui.Config (Config)
import Gui.WishList.Dom (renderWishList)

import Data.Json as Json
import Dom as Dom
import Gui.I18n as I18n

main :: Config -> Effect (Dom.Error ()) Unit
main config = do
  window <- Dom.window
  document <- Dom.windowDocument window
  container <- Dom.documentGetElementById document "container"
                 >>= maybe (throw (Dom.newError "Cannot find #container")) pure

  element <- renderWishList I18n.nlNl document
               [ { name: Just "Schoen", imageUrl: Nothing }
               , { name: Just "Rok",    imageUrl: Nothing }
               , { name: Just "Kerbal", imageUrl: Nothing } ]
  Dom.nodeAppendChild container element

  xhr <- Dom.newXmlHttpRequest
  Dom.eventTargetAddEventListener xhr "load" \_ -> scare do
    response <- Dom.xmlHttpRequestGetResponseText xhr
                  >>= maybe (throw (Dom.newError "No response text")) pure
    json <- maybe (throw (Dom.newError "Bad JSON")) pure (Json.parse response)
    pure unit
  Dom.xmlHttpRequestOpen xhr "GET" (config.apiUrl <> "/")
  Dom.xmlHttpRequestSend xhr

  pure unit

scare :: forall a e. Effect (Dom.Error a) Unit -> Effect e Unit
scare action = action `rid` (Dom.alert << Dom.errorMessage)
