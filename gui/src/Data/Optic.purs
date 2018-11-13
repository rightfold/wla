module Data.Optic
  ( Optic

  , Prism
  , Prism'
  , prism
  ) where

import Control.Semigroupoid (id)
import Data.Either (Either, either)
import Data.Functor (class Choice, lpmap, rpmap, right)

--------------------------------------------------------------------------------

type Optic p s t a b = p a b -> p s t

--------------------------------------------------------------------------------

type Prism s t a b = forall p. Choice p => Optic p s t a b
type Prism' s a = Prism s s a a

prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism fbt fse pab = lpmap fse (rpmap (either id id) (right (rpmap fbt pab)))
