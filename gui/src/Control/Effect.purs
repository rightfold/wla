module Control.Effect
  ( Effect
  ) where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Error (class MonadBlunder, class MonadError, class MonadThrow, rid, throw)
import Control.Semigroupoid ((<<))
import Data.Bifunctor (class Bifunctor)
import Data.Functor (class Functor, map)

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

instance bifunctorEffect :: Bifunctor Effect where
  lmap f action = action `rid` (throw << f)
  rmap = map

instance monadThrowEffect :: MonadThrow e (Effect e) where
  throw = throwF

instance monadErrorEffect :: MonadError e (Effect e) where
  catch = ridF

instance monadBlunderEffect :: MonadBlunder e (Effect v) (Effect e) where
  rid = ridF

foreign import mapF :: forall e a b. (a -> b) -> Effect e a -> Effect e b
foreign import bindF :: forall e a b. Effect e a -> (a -> Effect e b) -> Effect e b
foreign import applyF :: forall e a b. Effect e (a -> b) -> Effect e a -> Effect e b
foreign import pureF :: forall e a. a -> Effect e a
foreign import throwF :: forall e a. e -> Effect e a
foreign import ridF :: forall e v a. Effect e a -> (e -> Effect v a) -> Effect v a
