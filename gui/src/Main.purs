module Main
  ( main
  ) where

import Data.Functor ((>>=), bind, discard, pure)
import Control.Semigroupoid ((<<))
import Control.Effect (Effect)
import Control.Monad.Error (rid, throw)
import Data.Semigroup ((<>))
import Data.Either (Either (..), maybe)
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
               [ { name: Right "Schoen", imageUrl: Left unit }
               , { name: Right "Rok",    imageUrl: Left unit }
               , { name: Right "Kerbal", imageUrl: Left unit } ]
  Dom.nodeAppendChild container element

  xhr <- Dom.newXmlHttpRequest
  Dom.eventTargetAddEventListener xhr "load" \_ -> scare do
    response <- Dom.xmlHttpRequestGetResponseText xhr
                  >>= maybe (throw (Dom.newError "No response text")) pure
    json <- maybe (throw (Dom.newError "Bad JSON")) pure (Json.parse response)
    pure unit
  Dom.xmlHttpRequestOpen xhr "GET" (config.apiUrl <> "/wish-list.json")
  Dom.xmlHttpRequestSend xhr

  pure unit

scare :: forall a e. Effect (Dom.Error a) Unit -> Effect e Unit
scare action = action `rid` (Dom.alert << Dom.errorMessage)
