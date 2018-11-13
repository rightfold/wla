module Main
  ( main
  ) where

import Control.Applicative (pure)
import Control.Bind ((>>=), bind, discard)
import Control.Effect (Effect)
import Control.Monad.Error (throw)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe (..), maybe)
import Data.Unit (Unit, unit)
import Gui.Config (Config)
import Gui.WishList.Dom (renderWishList)

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
  Dom.eventTargetAddEventListener xhr "load" \_ ->
    pure unit
  Dom.xmlHttpRequestOpen xhr "GET" (config.apiUrl <> "/")
  Dom.xmlHttpRequestSend xhr

  pure unit
