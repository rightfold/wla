module Data.Json
  ( Json
  , parse
  , stringify
  ) where

import Data.Maybe (Maybe (..))

-- | JSON value, same representation as that returned by the ECMAScript
-- | quasi-function JSON.parse.
foreign import data Json :: Type

-- | Parse JSON.
parse :: String -> Maybe Json
parse = parseF Nothing Just
foreign import parseF :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> String -> Maybe Json

-- | Format JSON.
foreign import stringify :: Json -> String
