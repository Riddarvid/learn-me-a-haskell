{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Alternative (altMaybeTest) where
import           Control.Applicative (Alternative (empty, many, (<|>)), liftA)
import           Control.Monad       (MonadPlus, ap, guard)

altMaybeTest :: IO ()
altMaybeTest = do
  let j1 = Just 5 :: Maybe Int
  let j2 = Just 7 :: Maybe Int
  let j3 = Nothing :: Maybe Int
  putStr "Empty: "
  print (empty :: Maybe Int)
  putStrLn ""
  putStrLn "<|>: "
  print (j1 <|> j2)
  print (j2 <|> j1)
  print (j1 <|> j3)
  print (j3 <|> j1)
  print (j2 <|> j3)
  print (j3 <|> j2)
  putStrLn ""
  putStrLn "some"
  print $ head <$> many j3
  --print $ some j2
  --print $ some j3

-----------------------------------------------

data MyList a = MLE | MLC a (MyList a)

(+++) :: MyList a -> MyList a -> MyList a
MLE +++ ys      = ys
MLC x xs +++ ys = MLC x (xs +++ ys)

myConcat :: MyList (MyList a) -> MyList a
myConcat = foldr (+++) MLE

instance Foldable MyList where
  foldMap :: Monoid m => (a -> m) -> MyList a -> m
  foldMap _ MLE        = mempty
  foldMap f (MLC x xs) = mappend (f x) (foldMap f xs)

instance Functor MyList where
  fmap :: (a -> b) -> MyList a -> MyList b
  fmap = liftA

instance Applicative MyList where
  pure :: a -> MyList a
  pure x = MLC x MLE
  (<*>) :: MyList (a -> b) -> MyList a -> MyList b
  (<*>) = ap

instance Monad MyList where
  (>>=) :: MyList a -> (a -> MyList b) -> MyList b
  MLE >>= _ = MLE
  xs >>= f  = myConcat $ fmap f xs

instance Alternative MyList where
  empty :: MyList a
  empty = MLE
  (<|>) :: MyList a -> MyList a -> MyList a
  (<|>) = (+++)

instance MonadPlus MyList -- Nothing more needed since MyList is Alternative and Monad.

-----------------------------------------------------------------------------------------

myGuard :: Alternative f => Bool -> f ()
myGuard True  = pure ()
myGuard False = empty

-- If the value supplied to guard is True, then we pass the guard changing nothing
-- and returning nothing.
-- If the value instead is False, then we produce the empty value, based on the implementation
-- of Alternative for the data type.

-----------------------------------------------------------------------------

-- A knight's quest

type Pos = (Int, Int)

knightOffsets :: [Pos]
knightOffsets =
  [(-2, -1), (-2, 1),
  (-1, -2), (-1, 2),
  (1, -2), (1, 2),
  (2, -1), (2, 1)]

move :: Pos -> Pos -> Pos
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

reachable :: Pos -> [Pos]
reachable pos = do
  (col', row') <- map (move pos) knightOffsets
  guard (col' `elem` [1..8] && row' `elem` [1..8])
  return (col', row')
