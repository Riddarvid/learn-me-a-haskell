{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Writer (gangTest, gcdTest, fclTest, fcdlTest) where
import           Control.Arrow         (Arrow (first))
import           Control.Monad.Writer  (MonadWriter (tell), Writer, execWriter,
                                        runWriter)
import           Data.Functor          (($>))
import           Data.Functor.Identity (Identity (runIdentity))

newtype MyWriterT w m a = MyWriterT (m (a, w))

myRunWriterT :: MyWriterT w m a -> m (a, w)
myRunWriterT (MyWriterT ma) = ma

type MyWriter w = MyWriterT w Identity

myRunWriter :: MyWriter w a -> (a, w)
myRunWriter = runIdentity . myRunWriterT

instance (Functor m) => Functor (MyWriterT w m) where
  fmap :: (a -> b) -> MyWriterT w m a -> MyWriterT w m b
  fmap f (MyWriterT ma) = MyWriterT (fmap (first f) ma)

instance (Monoid w, Applicative m) => Applicative (MyWriterT w m) where
  pure :: forall a . a -> MyWriterT w m a
  pure x = MyWriterT (pure (x, mempty))
  (<*>) :: forall a b . MyWriterT w m (a -> b) -> MyWriterT w m a -> MyWriterT w m b
  MyWriterT mf <*> MyWriterT mx = MyWriterT $ k <$> mf <*> mx
    where
      k :: (a -> b, w) -> (a, w) -> (b, w)
      k (f, w1) (x, w2) = (f x, mappend w1 w2)

instance (Monoid w, Monad m) => Monad (MyWriterT w m) where
  (>>=) :: forall a b . MyWriterT w m a -> (a -> MyWriterT w m b) -> MyWriterT w m b
  MyWriterT ma >>= f = MyWriterT $ do
    (a, wa) <- ma
    let (MyWriterT mb') = f a
    (b, wb) <- mb'
    return (b, mappend wa wb)

class (Functor m) => MyMonadWriter w m | m -> w where
  myWriter :: (a, w) -> m a
  myWriter (a, w) = myTell w $> a
  myTell :: w -> m ()
  myTell w = myWriter ((), w)
  myListen :: m a -> m (a, w)
  myPass :: m (a, w -> w) -> m a

-- Here we see why it is logical that w has a functional dependency on m in the class declaration:
-- Since MyWriterT is parameterized over w, we already know what w "must" be.
instance (Applicative m) => MyMonadWriter w (MyWriterT w m) where
  myTell :: w -> MyWriterT w m ()
  myTell w = MyWriterT (pure ((), w))
  myListen :: MyWriterT w m a -> MyWriterT w m (a, w)
  myListen (MyWriterT ma) = MyWriterT ((\(a, w) -> ((a, w), w)) <$> ma)
  myPass :: MyWriterT w m (a, w -> w) -> MyWriterT w m a
  myPass (MyWriterT ma) = MyWriterT $ (\((x, f), w) -> (x, f w)) <$> ma

-----------------------------------------------------------------------------------

-- Example

isBigGang :: Int -> MyWriter String Bool
isBigGang n = myWriter (n > 9, "Compared gang size to 9.\n")

gangExample :: MyWriter String Bool
gangExample = do
  b1 <- isBigGang 5
  b2 <- isBigGang 20
  --(b3, compLog) <- listen $ isBigGang 10
  myTell "Check if both are big.\n"
  return (b1 && b2)

gangTest :: IO ()
gangTest = do
  let (res, log') = myRunWriter gangExample
  putStrLn $ "Result: " ++ show res
  putStrLn $ "Log:\n" ++ log'

-----------------------------------------------------------------------------

gcd' :: (Integral a, Show a) => a -> a -> Writer [String] a
gcd' a 0 = do
  tell ["Finished with " ++ show a]
  return a
gcd' a b = do
  tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
  gcd' b (a `mod` b)

gcdTest :: Int -> Int -> IO ()
gcdTest a b = do
  let (res, log') = runWriter $ gcd' a b
  putStrLn $ "Result: " ++ show res
  putStrLn $ "Log:\n" ++ unlines log'

-----------------------------------------

-- Diff lists

newtype DiffList a = DiffList ([a] -> [a])

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (<>) :: DiffList a -> DiffList a -> DiffList a
  DiffList f <> DiffList g = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty :: DiffList a
  mempty = DiffList id

-- This is very slow since we append a lot of short strings to the end of a really long one
finalCountdownList :: Int -> Writer [String] ()
finalCountdownList 0 = tell ["0"]
finalCountdownList n = do
  finalCountdownList (n - 1)
  tell [show n]

fclTest :: Int -> IO ()
fclTest n = mapM_ putStrLn $ execWriter $ finalCountdownList n

-- This is fast, since difflist makes the ++ operations associate to the right.
finalCountdownDiffList :: Int -> Writer (DiffList String) ()
finalCountdownDiffList 0 = tell $ toDiffList ["0"]
finalCountdownDiffList n = do
  finalCountdownDiffList (n - 1)
  tell $ toDiffList [show n]

fcdlTest :: Int -> IO ()
fcdlTest n = mapM_ putStrLn $ fromDiffList $ execWriter $ finalCountdownDiffList n
