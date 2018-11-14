module Data.Optic
  ( Optic

  , Prism
  , Prism'
  , prism
  , prism'
  ) where

import Control.Semigroupoid (id)
import Data.Either (Either (..), Maybe, either, maybe)
import Data.Functor (class Choice, lpmap, rpmap, right)

--------------------------------------------------------------------------------

type Optic p s t a b = p a b -> p s t

--------------------------------------------------------------------------------

type Prism s t a b = forall p. Choice p => Optic p s t a b
type Prism' s a = Prism s s a a

prism :: forall s t a b. (s -> Either t a) -> (b -> t) -> Prism s t a b
prism fse fbt pab = lpmap fse (rpmap (either id id) (right (rpmap fbt pab)))

prism' :: forall s a b. (s -> Maybe a) -> (b -> s) -> Prism s s a b
prism' fsm fbt = prism (\s -> maybe (Left s) Right (fsm s)) fbt
