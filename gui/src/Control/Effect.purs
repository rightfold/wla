module Control.Effect
  ( Effect
  ) where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Error (class MonadThrow)
import Data.Functor (class Functor)

foreign import data Effect :: Type -> Type -> Type

instance functorEffect :: Functor (Effect e) where
  map = mapF

instance bindEffect :: Bind (Effect e) where
  bind = bindF

instance applyEffect :: Apply (Effect e) where
  apply = applyF

instance applicativeEffect :: Applicative (Effect e) where
  pure = pureF

instance monadEffect :: Monad (Effect e)

instance monadThrowEffect :: MonadThrow e (Effect e) where
  throw = throwF

foreign import mapF :: forall e a b. (a -> b) -> Effect e a -> Effect e b
foreign import bindF :: forall e a b. Effect e a -> (a -> Effect e b) -> Effect e b
foreign import applyF :: forall e a b. Effect e (a -> b) -> Effect e a -> Effect e b
foreign import pureF :: forall e a. a -> Effect e a
foreign import throwF :: forall e a. e -> Effect e a
