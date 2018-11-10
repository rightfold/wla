module Dom
  ( Foreign

  , kind Interface
  , DOCUMENT
  , ELEMENT
  , ERROR
  , NODE
  , WINDOW

  , Document
  , Element
  , Error
  , Node
  , Window

  , documentCreateElement
  , documentGetElementById
  , newError
  , nodeAppendChild
  , nodeSetTextContent
  , window
  , windowDocument
  ) where

import Control.Effect (Effect)
import Data.Maybe (Maybe (..))
import Data.Unit (Unit)

foreign import data Foreign :: # Interface -> Type
foreign import kind Interface

foreign import data NODE :: Interface
foreign import data DOCUMENT :: Interface
foreign import data ELEMENT :: Interface
foreign import data ERROR :: Interface
foreign import data WINDOW :: Interface

type Node     a = Foreign (node     :: NODE     | a)
type Document a = Node    (document :: DOCUMENT | a)
type Element  a = Node    (element  :: ELEMENT  | a)
type Error    a = Foreign (error    :: ERROR    | a)
type Window   a = Foreign (window   :: WINDOW   | a)

documentGetElementById :: forall a. Document a -> String -> Effect (Error ()) (Maybe (Element ()))
documentGetElementById = documentGetElementByIdF Nothing Just

foreign import documentCreateElement :: forall a. Document a -> String -> Effect (Error ()) (Element ())
foreign import documentGetElementByIdF :: forall a. (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Document a -> String -> Effect (Error ()) (Maybe (Element ()))
foreign import newError :: String -> Error ()
foreign import nodeAppendChild :: forall a b. Node a -> Node b -> Effect (Error ()) Unit
foreign import nodeSetTextContent :: forall a. Node a -> String -> Effect (Error ()) Unit
foreign import window :: Effect (Error ()) (Window ())
foreign import windowDocument :: forall a. Window a -> Effect (Error ()) (Document ())
