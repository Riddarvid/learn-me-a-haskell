{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Functors (
  funcTest,
  funcMonadtest
) where
import           Data.Char                  (toUpper)
import           Data.Functor.Contravariant (Contravariant (contramap))

data MyList a = MyNil | MyCons a (MyList a)

instance Functor MyList where
  fmap :: (a -> b) -> MyList a -> MyList b
  fmap _ MyNil         = MyNil
  fmap f (MyCons x xs) = MyCons (f x) (fmap f xs)

newtype MyId a = MkMyId a

{-instance Functor MyId where
  fmap :: (a -> b) -> MyId a -> MyId b
  fmap f (MkMyId x) = MkMyId (f x)-}

-- Functor instance defined in terms of Applicative
{-instance Functor MyId where
  fmap :: (a -> b) -> MyId a -> MyId b
  fmap f myId = pure f <*> myId-}

-- Functor instance defined in terms of Monad
instance Functor MyId where
  fmap :: (a -> b) -> MyId a -> MyId b
  fmap f xAction = do
    x <- xAction
    return (f x)

{-instance Applicative MyId where
  pure :: a -> MyId a
  pure = MkMyId
  (<*>) :: MyId (a -> b) -> MyId a -> MyId b
  (MkMyId f) <*> (MkMyId x) = MkMyId (f x)-}

-- Applicative defined in terms of Monad
instance Applicative MyId where
  pure :: a -> MyId a
  pure = return
  (<*>) :: MyId (a -> b) -> MyId a -> MyId b
  fAction <*> xAction = do
    f <- fAction
    x <- xAction
    return (f x)

instance Monad MyId where
  (>>=) :: MyId a -> (a -> MyId b) -> MyId b
  (MkMyId x) >>= f = f x

newtype MyFunc a b = MyFunc (a -> b)

instance Functor (MyFunc c) where
  fmap :: (a -> b) -> MyFunc c a -> MyFunc c b
  fmap g (MyFunc f) = MyFunc (g . f) -- We first run our function, then apply a function to the output

instance Applicative (MyFunc c) where
  pure :: a -> MyFunc c a
  pure x = MyFunc (const x)
  (<*>) :: MyFunc c (a -> b) -> MyFunc c a -> MyFunc c b
  (MyFunc cToF) <*> (MyFunc cToX) = MyFunc (\c -> cToF c $ cToX c)

instance Monad (MyFunc c) where
  (>>=) :: MyFunc c a -> (a -> MyFunc c b) -> MyFunc c b
  (MyFunc cToX) >>= f = MyFunc (\c -> let (MyFunc f') = f $ cToX c in f' c)
  -- Given a c, apply the first function to it to get a value of type a.
  -- Use that value to get a function that when given a c produces a b.
  -- Supply the same c given to the first function to this function, to get a b.
  -- We now have a function from c to b, which is what we wanted.

newtype ContraFunc a b = CF (b -> a)

instance Contravariant (ContraFunc c) where
  contramap :: (b -> a) -> ContraFunc c a -> ContraFunc c b
  contramap g (CF f) = CF (f . g) -- We first apply a function to the input, then run our function

funcTest :: String
funcTest = fmap (show <$> (+3)) (*100) (42 :: Integer)

funcMonadtest :: String -> String
funcMonadtest = do
  x <- reverse
  y <- map toUpper
  return (x ++ y)

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither r) where
  fmap :: (a -> b) -> MyEither r a -> MyEither r b
  fmap _ (MyLeft x)  = MyLeft x
  fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither r) where
  pure :: a -> MyEither r a
  pure = MyRight
  (<*>) :: MyEither r (a -> b) -> MyEither r a -> MyEither r b
  (MyLeft x) <*> _        = MyLeft x
  (MyRight f) <*> myRight = f <$> myRight

instance Monad (MyEither r) where
  (>>=) :: MyEither r a -> (a -> MyEither r b) -> MyEither r b
  (MyLeft x) >>= _  = MyLeft x
  (MyRight x) >>= f = f x

------------------------ Applicative, not a monad

newtype Phantom o a = Phantom o
-- This datatype is parameterised over two data types, but we only actually use one of them.

instance Functor (Phantom o) where
  fmap :: (a -> b) -> Phantom o a -> Phantom o b
  fmap _ (Phantom o) = Phantom o
-- Since we don't actually have a value of type a, there is no mapping to be done.
-- This obeys the functor laws, since no matter what function we map over an instance phantom,
-- it stays the same.

instance (Monoid o) => Applicative (Phantom o) where
  pure :: a -> Phantom o a
  pure _ = Phantom mempty
  -- We're given a value of type a to put into the functor, but since we don't actually have a value
  -- of type a in the functor, we simply throw it away.
  (<*>) :: Phantom o (a -> b) -> Phantom o a -> Phantom o b
  Phantom o1 <*> Phantom o2 = Phantom (mappend o1 o2)
-- It can easily be shown that this obeys the applicative laws.

instance (Monoid o) => Monad (Phantom o) where
  (>>=) :: Phantom o a -> (a -> Phantom o b) -> Phantom o b
  Phantom _o >>= _f = undefined
  -- Since we don't actually have a value of type a to supply to function _f, we will not be able to
  -- get a value of type Phantom o b that way. However, any value Phantom o can act as any type with
  -- regards to type parameter a. This means that we can simply create our own value. We must, however,
  -- ensure that the monad laws hold.
  -- By the left identity law we have return x >>= k = k x.
  -- However, since we can't depend on k, it is obvious that there will be instances where this
  -- law does not hold. Thus we have shown an example of an applicative functor that is not a monad.


-- Small experiment to understand the usefulness of applicative functors.

myApp :: Functor f => f (a -> b) -> a -> f b
myApp app val = fmap (\f -> f val) app

myAppTest :: Maybe Integer
myAppTest = Just (\x y z -> x + y + z) `myApp` 5 `myApp` 3 `myApp` 2

----------------------------

newtype MyZipList a = MyZL [a]

instance Functor MyZipList where
  fmap :: (a -> b) -> MyZipList a -> MyZipList b
  fmap f (MyZL ls) = MyZL (fmap f ls)

instance Applicative MyZipList where
  pure :: a -> MyZipList a
  pure = MyZL . repeat
  (<*>) :: MyZipList (a -> b) -> MyZipList a -> MyZipList b
  (MyZL fs) <*> (MyZL xs) = MyZL (zipWith ($) fs xs)

-------------------------

myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

myLiftA :: (Applicative f) => (a -> b) -> f a -> f b
myLiftA f a = pure f <*> a
