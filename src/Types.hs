{-# LANGUAGE InstanceSigs #-}

-- Some examples for types and kinds.

module Types (
  MyMaybe(..),
  MyEither(..),
  Tofu(..),
  Frank(..),
  Barry(..)
) where

data MyMaybe a = MyNothing | MyJust a

-- MyMaybe is a type constructor with kind * -> * (it takes one type parameter)
-- MyMaybe Int is a type constructor with kind * (it takes no type parameters and is therefore a concrete type)
-- Nothing is a value constructor with type MyMaybe a
-- MyJust is a value constructor with type a -> MyMaybe a

-- A type constructor must have kind * -> * to be able to be an instance of the Functor typeclass.
-- Therefore MyMaybe can be made an instance of Functor, but MyMaybe a cannot.

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing  = MyNothing
  fmap f (MyJust x) = MyJust (f x)

-- Applicative also requires a type constructor of kind * -> *

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = MyJust
  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  MyNothing <*> _        = MyNothing
  (MyJust f) <*> myMaybe = f <$> myMaybe

-- Monad also requires * -> *

instance Monad MyMaybe where
  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  MyNothing >>= _  = MyNothing
  (MyJust x) >>= f = f x

---------------------------------

data MyEither a b = MyLeft a | MyRight b

-- Behaviour: fmap does nothing when applied to a Left value.
instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap _ (MyLeft x)  = MyLeft x
  fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither a) where
  pure :: b -> MyEither a b
  pure = MyRight
  (<*>) :: MyEither a (b -> c) -> MyEither a b -> MyEither a c
  (MyLeft x) <*> _         = MyLeft x
  (MyRight f) <*> myEither = f <$> myEither

instance Monad (MyEither a) where
  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  (MyLeft x) >>= _  = MyLeft x
  (MyRight x) >>= f = f x

-----------------------------------

class Tofu t where
  tofu :: j a -> t a j

-- From this specification, we can infere a few things. If a has kind k, then j must have type k -> *, since
-- j a is used as a concrete type in the type declaration for tofu. From this, we see that t must have type
-- k -> (k -> *) -> *, by the same logic. If we assume that a has kind *, then t has kind * -> (* -> *) -> *

newtype Frank a j = Frank (j a)

-- In the Frank data type, it is clear that if a has kind k, then j has kind (k -> *), which is exactly what we need.

instance Tofu Frank where
  tofu :: j a -> Frank a j
  tofu = Frank

--------------------------------

data Barry t k p = Barry {
  yabba :: p,
  dabba :: t k
}

-- p has kind *, k has kind k', t has kind (k' -> *)

instance Functor (Barry t k) where
  fmap :: (p -> q) -> Barry t k p -> Barry t k q
  fmap f barry = barry{yabba = f $ yabba barry}
