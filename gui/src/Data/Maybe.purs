module Data.Maybe
  ( Maybe (..)
  , maybe
  , fromMaybe
  , coalesceMaybe, (??)
  ) where

data Maybe a
  = Nothing
  | Just a

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ j (Just x) = j x

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe n Nothing  = n
fromMaybe _ (Just x) = x

coalesceMaybe :: forall a. Maybe a -> a -> a
coalesceMaybe Nothing  n = n
coalesceMaybe (Just x) _ = x
infixl 2 coalesceMaybe as ??