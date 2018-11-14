module Main
  ( main
  ) where

import Data.Functor ((>>=), bind, discard, pure)
import Control.Semigroupoid (($), (<<))
import Control.Effect (Effect)
import Control.Monad.Error (rid, throw)
import Data.Semigroup ((<>))
import Data.Either (Either (..), either, maybe)
import Data.Unit (Unit, unit)
import Gui.Config (Config)
import Gui.WishList.Dom (renderWishList)

import Data.Json as Json
import Dom as Dom
import Gui.I18n as I18n
import Network.Ajax as Ajax

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

  let req = { method: "GET", url: config.apiUrl <> "/wish-list.json" }
  Ajax.request' req $ scare << \result -> do
    res <- either throw pure result
    json <- maybe (throw (Dom.newError "Bad JSON")) pure (Json.parse res.body)
    pure unit

  pure unit

scare :: forall a e. Effect (Dom.Error a) Unit -> Effect e Unit
scare action = action `rid` (Dom.alert << Dom.errorMessage)
