module Data.Functor
  ( class Functor
  , map, (<$>)
  , void

  , class Bind
  , class Discard
  , bind, (>>=)
  , discard

  , class Apply
  , apply, (<*>)
  , applyFirst, (<*)
  , applySecond, (*>)

  , class Applicative
  , pure
  , pu
  , ppu

  , class Monad

  , class Bifunctor
  , lmap
  , rmap

  , class Profunctor
  , rpmap
  , lpmap

  , class Choice
  , left
  , right
  ) where

import Control.Semigroupoid ((<<))
import Data.Either (type (+), Either (..))
import Data.Unit (Unit, unit)

--------------------------------------------------------------------------------

-- | Functor from Purs to a subcategory of Purs where f is applied to every
-- | object.
-- |
-- |  - forall xs. map id xs = xs
-- |  - forall f g. map f << map g = map (f << g)
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
infixl 4 map as <$>

instance functorFunction :: Functor (Function i) where
  map = (<<)

instance functorEither :: Functor (Either a) where
  map _ (Left a)  = Left a
  map f (Right a) = Right (f a)

-- | Map morphism to terminal object.
void :: forall f a. Functor f => f a -> f Unit
void = map \_ -> unit

--------------------------------------------------------------------------------

class Functor f <= Bind f where
  bind :: forall a b. f a -> (a -> f b) -> f b
infixl 1 bind as >>=

class Discard a
instance discardUnit :: Discard Unit

discard :: forall a f b. Discard a => Bind f => f a -> (a -> f b) -> f b
discard = bind

--------------------------------------------------------------------------------

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
infixl 4 apply as <*>

instance applyFunction :: Apply (Function i) where
  apply f g x = f x (g x)

applyFirst :: forall f a. Apply f => f a -> f Unit -> f a
applyFirst a b = (\x _ -> x) <$> a <*> b
infixl 4 applyFirst as <*

applySecond :: forall f a. Apply f => f Unit -> f a -> f a
applySecond a b = (\_ x -> x) <$> a <*> b
infixl 4 applySecond as *>

--------------------------------------------------------------------------------

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeFunction :: Applicative (Function i) where
  pure x = \_ -> x

-- | Short-hand for pure unit.
pu :: forall f. Applicative f => f Unit
pu = pure unit

-- | Short-hand for pure (pure unit), and the very common const (pure unit).
ppu :: forall f g. Applicative f => Applicative g => f (g Unit)
ppu = pure pu

--------------------------------------------------------------------------------

class (Applicative f, Bind f) <= Monad f

--------------------------------------------------------------------------------

class Bifunctor f where
  lmap :: forall a b x. (a -> b) -> f a x -> f b x
  rmap :: forall a b x. (a -> b) -> f x a -> f x b

--------------------------------------------------------------------------------

class Profunctor p where
  lpmap :: forall a b x. (b -> a) -> p a x -> p b x
  rpmap :: forall a b x. (a -> b) -> p x a -> p x b

instance profunctorFunction :: Profunctor Function where
  lpmap f g x = g (f x)
  rpmap f g x = f (g x)

--------------------------------------------------------------------------------

class Profunctor p <= Choice p where
  left  :: forall a b x. p a b -> p (a + x) (b + x)
  right :: forall a b x. p a b -> p (x + a) (x + b)
