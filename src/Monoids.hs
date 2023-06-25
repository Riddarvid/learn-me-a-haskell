{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Monoids (
  MyPair(..),
  funTest
) where
import           Data.Coerce (coerce)
import           Data.Monoid (Dual (Dual, getDual), Endo (Endo, appEndo))

-----------------------------------------------------------

newtype MyPair a b = MP { unMyPair :: (a, b)}

instance (Semigroup a, Semigroup b) => Semigroup (MyPair a b) where
  (<>) :: MyPair a b -> MyPair a b -> MyPair a b
  MP (x1, y1) <> MP (x2, y2) = MP (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (MyPair a b) where
  mempty :: MyPair a b
  mempty = MP (mempty, mempty)

----------------------------------------------------------

newtype MyMaybe a = MM (Maybe a)

instance (Semigroup a) => Semigroup (MyMaybe a) where
  (<>) :: MyMaybe a -> MyMaybe a -> MyMaybe a
  MM Nothing <> mm           = mm
  mm <> MM Nothing           = mm
  MM (Just x) <> MM (Just y) = MM (Just (x <> y))

instance (Semigroup a) => Monoid (MyMaybe a) where
  mempty :: MyMaybe a
  mempty = MM Nothing

newtype MyMaybe2 a = MM2 (Maybe a)

instance (Semigroup a) => Semigroup (MyMaybe2 a) where
  (<>) :: MyMaybe2 a -> MyMaybe2 a -> MyMaybe2 a
  MM2 Nothing <> _             = MM2 Nothing
  _ <> MM2 Nothing             = MM2 Nothing
  MM2 (Just x) <> MM2 (Just y) = MM2 (Just (x <> y))

instance (Monoid a) => Monoid (MyMaybe2 a) where
  mempty :: MyMaybe2 a
  mempty = MM2 (Just mempty)

---------------------------------------------------

newtype MyProduct a = MProd { unProd :: a }

instance Num a => Semigroup (MyProduct a) where
  (<>) :: MyProduct a -> MyProduct a -> MyProduct a
  (<>) = coerce ((*) :: a -> a -> a)

instance Num a => Monoid (MyProduct a) where
  mempty :: MyProduct a
  mempty = MProd 1

instance Functor MyProduct where
  fmap :: (a -> b) -> MyProduct a -> MyProduct b
  fmap = coerce

newtype MySum a = MSum { unSum :: a }

instance Num a => Semigroup (MySum a) where
  (<>) :: MySum a -> MySum a -> MySum a
  (<>) = coerce ((+) :: a -> a -> a)

instance Num a => Monoid (MySum a) where
  mempty :: MySum a
  mempty = MSum 0

instance Functor MySum where
  fmap :: (a -> b) -> MySum a -> MySum b
  fmap = coerce

-----------------------------------------

data MyOrdering = MyLT | MyEQ | MyGT

-- This definition means that mconcat $ compare will compare the elements in lexiographic order
instance Semigroup MyOrdering where
  (<>) :: MyOrdering -> MyOrdering -> MyOrdering
  MyLT <> _ = MyLT
  MyGT <> _ = MyGT
  MyEQ <> y = y

----------------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty          = mempty
  foldMap f (Node x lt rt) = foldMap f lt <> f x <> foldMap f rt

-----------------------------------------

newtype MyEndo a = MyEndo {appMyEndo :: a -> a}

instance Semigroup (MyEndo a) where
  (<>) :: MyEndo a -> MyEndo a -> MyEndo a
  MyEndo f <> MyEndo g = MyEndo (f . g)

instance Monoid (MyEndo a) where
  mempty :: MyEndo a
  mempty = MyEndo id

-- For every element x of type a in the structure, replace it with a function b -> b.
-- b -> b (wrapped in Endo) is a monoid, therefore we can combine them using foldmap,
-- basically applying them one after the other.
myFoldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
myFoldr f acc xs = appMyEndo bigFun acc
  where
    bigFun = foldMap (MyEndo . f) xs

-- The Endo type is needed since we can't write instance Monoid ((->) a a), since
-- the type variables must be distinct. We therefore introduce the type Endo
-- to let us define a monoid instance for functions from one type to the same type.
-- I belive this is what is known as an endomorphism.

newtype MyDual a = MyDual a

instance (Semigroup a) => Semigroup (MyDual a) where
  (<>) :: MyDual a -> MyDual a -> MyDual a
  MyDual x <> MyDual y = MyDual (y <> x)

-- (MyDual x <> MyDual y) <> MyDual z = MyDual (y <> x) <> MyDual z = MyDual (z <> y <> x)
-- MyDual x <> (MyDual y <> MyDual z) = MyDual x <> MyDual (z <> y) = MyDual (z <> y <> x)

instance (Monoid a) => Monoid (MyDual a) where
  mempty :: MyDual a
  mempty = MyDual mempty

-- mempty <> MyDual x = MyDual mempty <> MyDual x = MyDual (x <> mempty) = MyDual x
-- MyDual x <> mempty = MyDual x <> MyDual mempty = MyDual (mempty <> x) = MyDual x

funTest :: (Integer, Integer)
funTest = (fun 0, reverseFun 0)
  where
    fun = appEndo $ mconcat (map Endo funs)
    reverseFun = appEndo $ getDual $ mconcat (map (Dual . Endo) funs)
    funs = [(+ 5), (* 3), \x -> x - 2]
