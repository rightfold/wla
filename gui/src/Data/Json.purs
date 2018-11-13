module Data.Json
  ( Json
  , parse
  , stringify
  ) where

import Data.Maybe (Maybe (..))

foreign import data Json :: Type

parse :: String -> Maybe Json
parse = parseF Nothing Just

foreign import parseF :: (forall x. Maybe x) -> (forall x. x -> Maybe x) -> String -> Maybe Json
foreign import stringify :: Json -> String
