{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Forall.ForallRankNTypes () where

putInList :: a -> [a]
putInList x = [x]

{-
liftTup1 f (a, b) = (f a, f b)

test1 = liftTup1 putInList (5, "Hello")-}

-- The above code does not compile. This is because ghc infers the type of liftTup
-- to be (a -> b) -> (a, a) -> (b, b).
-- That is, f is a function from a specific type to a specific other type. Therefore,
-- both values that it's applied to must have the same type.
-- Specifically, the error comes from the tuple we're trying to give it.
-- We need to specifically tell ghc that the function taken as an argument by liftTup
-- must be polymorphic.

{-liftTup2 :: (forall a b. a -> b) -> (a1, a2) -> (b1, b2)
liftTup2 f (a, b) = (f a, f b)

test2 = liftTup2 putInList (5, "hej")-}

-- This does not work either

liftTup3 :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
liftTup3 f (a, b) = (f a, f b)

test3 :: ([Integer], [String])
test3 = liftTup3 putInList (5, "hej")

-- This does work! The type of liftTup3 putInList becomes (a, b) -> ([a], [b])
-- RankNTypes lets us

{-
test4 :: (a -> f a) -> (a1, a2) -> (f a1, f a2)
test4 f (a, b) = (f a, f b)
-}

-- This does not work, since a and a1 are different types. We need the forall to specify
-- that f is a polymorphic function and that the type it will result in depends
-- on the value it is applied to.

-- This is a valid signature:
test5 :: (x -> f x) -> x -> f Bool
test5 = undefined
