{-# LANGUAGE RankNTypes
           , TupleSections
           #-}

import Data.Monoid
import Control.Applicative hiding(Const(..))

--The Identity Functor, which just does the same thing that normal functions do
data Identity a = Identity { runIdentity :: a }
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

--The Const Functor, which does nothing at all. Unless a is a monoid, then the Applicative does stuff
data Const a b = Const { getConst :: a }
instance Functor (Const a) where
  fmap _ (Const a) = Const a
instance (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const a' = Const (a <> a')


type PreSetter s t a b = (a ->          b) -> s ->          t
type Setter s t a b    = (a -> Identity b) -> s -> Identity t

preOver :: PreSetter s t a b -> (a -> b) -> s -> t
preOver l f s = l f s

over :: Setter s t a b -> (a -> b) -> s -> t
over l f s = runIdentity . l (Identity . f) $ s



type PreGetter s a  =                     s ->       a
type Getter s t a b = (a -> Const a b) -> s -> Const a t
--What is actually in the lens library.
type RealGetter s a = forall r t b. (a -> Const r b) -> s -> Const r t

preGet :: PreGetter s a -> s -> a
preGet l s = l s

get :: Getter s t a b -> s -> a
get l s = getConst . l Const $ s


--Any function that accepts a Getter or a Setter will accept a Lens
type Lens s t a b = forall f. Functor f
                  => (a -> f b) -> s -> f t


--
--Basic example lenses
--

--Get or set the first element of a pair
-- _1 :: Functor f => (a -> f b) -> (a,z) -> f (b,z)
_1 :: Lens (a,z) (b,z) a b
_1 u (a,z) = fmap (,z) (u a)

-- _2 :: Functor f => (a -> f b) -> (z,a) -> f (z,b)
_2 :: Lens (z,a) (z,b) a b
_2 u (z,a) = fmap (z,) (u a)

-- mapped :: Functor g => (a -> Identity b) -> g a -> Identity (g b)
mapped :: Functor g => Setter (g a) (g b) a b
mapped u x = Identity . fmap (runIdentity . u) $ x


--
--General constructor stuff
--

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens extract set u s = fmap (set s) (u . extract $ s)

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets f u s = Identity . f (runIdentity . u) $ s

to :: (s -> a) -> Getter s t a b
to f u s = Const (f s)

--Using these:
--_1 = lens fst (\(a,z) b -> (b,z))
--_2 = lens snd (\(z,a) b -> (z,b))
--mapped = sets fmap
