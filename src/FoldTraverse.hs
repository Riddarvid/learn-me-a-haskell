{-# LANGUAGE InstanceSigs #-}
module FoldTraverse (
  MyFoldable(..),
  MyTraversable(..),
  myTraverse_,
  myMapM_
) where
import           Control.Applicative (liftA2)

class MyFoldable t where
  myFoldr :: (a -> b -> b) -> b -> t a -> b

  myFoldMap :: (Monoid m) => (a -> m) -> t a -> m
  myFoldMap f = myFoldr (\x acc -> f x <> acc) mempty

instance MyFoldable [] where
  myFoldr :: (a -> b -> b) -> b -> [a] -> b
  myFoldr _ acc []     = acc
  myFoldr f acc (x:xs) = myFoldr f (f x acc) xs

class (MyFoldable t, Functor t) => MyTraversable t where
  -- Map each element of a structure to an action, then evaluate them from left to right and collect the results.
  myTraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  myTraverse f as = mySequenceA (fmap f as)

-- Given a structure of actions, evaluate them from left to right and collect the results.
  mySequenceA :: Applicative f => t (f a) -> f (t a)
  mySequenceA = myTraverse id

  -- Exists for historic reasons, since Applicative was earlier not a superclass of Monad
  myMapM :: Monad m => (a -> m b) -> t a -> m (t b)
  myMapM = myTraverse

  mySequence :: Monad m => t (m a) -> m (t a)
  mySequence = mySequenceA

instance MyTraversable [] where
  myTraverse :: (Applicative f) => (a -> f b) -> [a] -> f [b]
  myTraverse f = myFoldr (\a bs -> (:) <$> f a <*> bs) (pure [])

  mySequenceA :: (Applicative f) => [f a] -> f [a]
  mySequenceA = myFoldr (liftA2 (:)) (pure [])

myTraverse_ :: (MyFoldable t, Applicative f) => (a -> f b) -> t a -> f ()
myTraverse_ f = myFoldr g (pure ())
  where
    g x acc = f x *> acc
-- myTraverse_ f xs = myTraverse f xs *> pure () -- This works but needs t to be traversable,
-- not simply foldable

-- The reason we don't use myMapM is that it has stricter requirements on t,
-- namely that it should be an instance of MyTraversable. Here however, we only need it to be foldable.
myMapM_ :: (Monad m, MyFoldable t) => (a -> m b) -> t a -> m ()
myMapM_ f = myFoldr g (pure ())
  where
    g x acc = f x >> acc
