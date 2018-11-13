module Data.Json
  ( Json
  , parse
  , stringify

  , _Null
  , _Boolean
  , _Number
  , _String
  , _Array
  ) where

import Data.Maybe (Maybe (..))
import Data.Number.Finite (Finite)
import Data.Optic (Prism', prism')
import Data.Unit (Unit)

-- | JSON value, same representation as that returned by the ECMAScript
-- | quasi-function JSON.parse.
foreign import data Json :: Type

-- | Parse JSON.
parse :: String -> Maybe Json
parse = parseF Nothing Just
foreign import parseF :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> String -> Maybe Json

-- | Format JSON.
foreign import stringify :: Json -> String

_Null :: Prism' Json Unit
_Null = prism' (getNull Nothing Just) setNull

_Boolean :: Prism' Json Boolean
_Boolean = prism' (getBoolean Nothing Just) setBoolean

_Number :: Prism' Json Finite
_Number = prism' (getNumber Nothing Just) setNumber

_String :: Prism' Json String
_String = prism' (getString Nothing Just) setString

_Array :: Prism' Json (Array Json)
_Array = prism' (getArray Nothing Just) setArray

foreign import getNull :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Json -> Maybe Unit
foreign import setNull :: Unit -> Json

foreign import getBoolean :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Json -> Maybe Boolean
foreign import setBoolean :: Boolean -> Json

foreign import getNumber :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Json -> Maybe Finite
foreign import setNumber :: Finite -> Json

foreign import getString :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Json -> Maybe String
foreign import setString :: String -> Json

foreign import getArray :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> Json -> Maybe (Array Json)
foreign import setArray :: Array Json -> Json
