module Dom
  ( Foreign

  , kind Interface
  , DOCUMENT
  , ELEMENT
  , ERROR
  , EVENT
  , EVENTTARGET
  , NODE
  , WINDOW
  , XMLHTTPREQUEST

  , Document
  , Element
  , Error
  , Event
  , EventTarget
  , Node
  , Window
  , XmlHttpRequest

  , alert
  , documentCreateElement
  , documentGetElementById
  , errorMessage
  , eventTargetAddEventListener
  , newError
  , newXmlHttpRequest
  , nodeAppendChild
  , nodeSetTextContent
  , window
  , windowDocument
  , xmlHttpRequestGetResponseText
  , xmlHttpRequestOpen
  , xmlHttpRequestSend
  ) where

import Control.Effect (Effect)
import Data.Either (Either (..), Maybe)
import Data.Unit (Unit, unit)
import Data.Void (Void)

foreign import data Foreign :: # Interface -> Type
foreign import kind Interface

foreign import data DOCUMENT :: Interface
foreign import data ELEMENT :: Interface
foreign import data ERROR :: Interface
foreign import data EVENT :: Interface
foreign import data EVENTTARGET :: Interface
foreign import data NODE :: Interface
foreign import data WINDOW :: Interface
foreign import data XMLHTTPREQUEST :: Interface

type Document       a = Node        (document       :: DOCUMENT       | a)
type Element        a = Node        (element        :: ELEMENT        | a)
type Error          a = Foreign     (error          :: ERROR          | a)
type Event          a = Foreign     (event          :: EVENT          | a)
type EventTarget    a = Foreign     (eventTarget    :: EVENTTARGET    | a)
type Node           a = EventTarget (node           :: NODE           | a)
type Window         a = Foreign     (window         :: WINDOW         | a)
type XmlHttpRequest a = EventTarget (xmlHttpRequest :: XMLHTTPREQUEST | a)

documentGetElementById :: forall a. Document a -> String -> Effect (Error ()) (Maybe (Element ()))
documentGetElementById = documentGetElementByIdF (Left unit) Right

xmlHttpRequestGetResponseText :: forall a. XmlHttpRequest a -> Effect (Error ()) (Maybe String)
xmlHttpRequestGetResponseText = xmlHttpRequestGetResponseTextF (Left unit) Right

foreign import alert :: forall e. String -> Effect e Unit
foreign import documentCreateElement :: forall a. Document a -> String -> Effect (Error ()) (Element ())
foreign import documentGetElementByIdF :: forall a. (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Document a -> String -> Effect (Error ()) (Maybe (Element ()))
foreign import errorMessage :: forall a. Error a -> String
foreign import eventTargetAddEventListener :: forall a. EventTarget a -> String -> (Event () -> Effect Void Unit) -> Effect (Error ()) Unit
foreign import newError :: String -> Error ()
foreign import newXmlHttpRequest :: forall e. Effect e (XmlHttpRequest ())
foreign import nodeAppendChild :: forall a b. Node a -> Node b -> Effect (Error ()) Unit
foreign import nodeSetTextContent :: forall a. Node a -> String -> Effect (Error ()) Unit
foreign import window :: Effect (Error ()) (Window ())
foreign import windowDocument :: forall a. Window a -> Effect (Error ()) (Document ())
foreign import xmlHttpRequestGetResponseTextF :: forall a. (forall x. Maybe x) -> (forall x. x -> Maybe x) -> XmlHttpRequest a -> Effect (Error ()) (Maybe String)
foreign import xmlHttpRequestOpen :: forall a. XmlHttpRequest a -> String -> String -> Effect (Error ()) Unit
foreign import xmlHttpRequestSend :: forall a. XmlHttpRequest a -> Effect (Error ()) Unit
