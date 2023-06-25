{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Monads (writerTest, birdExample, birdExampleFail, failTest1, failTest2) where

newtype MyWriter m a = MyWriter (a, m)

instance Functor (MyWriter m) where
  fmap :: (a -> b) -> MyWriter m a -> MyWriter m b
  fmap f (MyWriter (x, m)) = MyWriter (f x, m)

instance Monoid m => Applicative (MyWriter m) where
  pure :: a -> MyWriter m a
  pure x = MyWriter (x, mempty)
  (<*>) :: MyWriter m (a -> b) -> MyWriter m a -> MyWriter m b
  MyWriter (f, m1) <*> MyWriter (x, m2) = MyWriter (f x, m1 <> m2)

instance Monoid m => Monad (MyWriter m) where
  (>>=) :: MyWriter m a -> (a -> MyWriter m b) -> MyWriter m b
  MyWriter (x, m1) >>= k = MyWriter (y, m1 <> m2)
    where
      MyWriter (y, m2) = k x

tell :: a -> MyWriter a ()
tell msg = MyWriter ((), msg)

logMessage :: String -> MyWriter [String] ()
logMessage msg = tell [msg]

runWriter :: MyWriter m a -> (a, m)
runWriter (MyWriter res) = res

writerEx :: MyWriter [String] Int
writerEx = do
  logMessage "Hello"
  logMessage "It's me"
  logMessage "Arvid"
  return 42

writerTest :: IO ()
writerTest = do
  putStrLn $ "Result: " ++ show result
  putStrLn $ "\nLog:\n\n" ++ unlines log'
  where
    (result, log') = runWriter writerEx

-----------------------------------------

data M a = N | J a

instance Functor M where
  fmap :: (a -> b) -> M a -> M b
  fmap f mx = do
    x <- mx
    pure (f x)

instance Applicative M where
  pure :: a -> M a
  pure = J
  (<*>) :: M (a -> b) -> M a -> M b
  mf <*> mx = do
    f <- mf
    x <- mx
    pure (f x)

instance Monad M where
  (>>=) :: M a -> (a -> M b) -> M b
  N >>= _   = N
  J x >>= f = f x

---------------------------------------

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs (l' - r) > 3 = Nothing
  | otherwise = Just (l', r)
  where
    l' = l + n

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (l - r') > 3 = Nothing
  | otherwise = Just (l, r')
  where
    r' = r + n

birdExample :: Maybe Pole
birdExample = Just (0, 0) >>= landRight 2 >>= landLeft 3 >>= landRight 1

birdExampleFail :: Maybe Pole
birdExampleFail = Just (0, 0) >>= landRight 2 >>= landLeft 3 >>= landRight 10

----------------------------------

-- fail test:

failTest1 :: Maybe String
failTest1 = do
  x <- Just "Hello"
  (a, b) <- Just ("good", "bye")
  return (x ++ a ++ b)

-- Results in Nothing, since Maybe is in MonadFail and fail _ = Nothing
failTest2 :: Maybe String
failTest2 = do
  x <- Just "Hello"
  (a : b) <- Just ""
  return (x ++ a : b)

-- Compilation error since Either is not in MonadFail and we have a failable pattern.
{-failTest3 :: Either String Int
failTest3 = do
  (a, b) <- Right ('a', 'b')
  (c : d) <- Right ""
  return $ length (a : b : c : d)-}
