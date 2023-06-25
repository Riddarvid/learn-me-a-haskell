{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module NewType (
  cbdEx,
  cbntEx,
  CoolType(..),
  ctTest1,
  ctTest2
) where

class MyFunctor  f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor ((,) c) where
  fmap :: (a -> b) -> (c, a) -> (c, b)
  fmap f (c, a) = (c, f a)

-- There is no way to make the first value in the tuple change with fmap, since only the last type
-- in the type constructor can vary. Solution: newtype where we change the order of types in the
-- type constructor:

newtype MyPair b a = MyPair {getMyPair :: (a, b)}

instance MyFunctor (MyPair c) where
  fmap :: (a -> b) -> MyPair c a -> MyPair c b
  fmap f (MyPair (a, c)) = MyPair (f a, c)

data CoolBoolData = CBD {getCBD :: Bool}

newtype CoolBoolNewType = CBNT {getCBNT :: Bool}

-- Will throw exception if called with undefined, since Haskell needs to evaluate the parameter
-- in order to perform pattern matching. This is necessary since data can potentially have
-- multiple value constructors.
cbdHello :: CoolBoolData -> String
cbdHello (CBD _) = "Hello"

-- Will not throw exception if called with undefined. Since newtypes can only have
-- a single value constructor, there is no need to actually do the pattern matching,
-- that is already taken care of by the compiler / type checker.
cbntHello :: CoolBoolNewType -> String
cbntHello (CBNT _) = "Hello"

cbdEx :: String
cbdEx = cbdHello undefined

cbntEx :: String
cbntEx = cbntHello undefined

data CoolType = CT Int Int

ctTest1 :: CoolType -> String
ctTest1 (CT v _) = if v == 7 then "Hello" else "Goodbye"

ctTest2 :: CoolType -> String
ctTest2 (CT _ v) = if v == 7 then "Hello" else "Goodbye"
