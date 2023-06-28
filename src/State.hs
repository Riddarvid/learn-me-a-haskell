{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module State (threeCoinsTest) where
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (MonadState (get, put, state), State,
                                         evalState, modify)
import           Data.Bifunctor         (Bifunctor (first))
import           System.Random          (Random (random), RandomGen, StdGen,
                                         mkStdGen)

newtype MyStateT s m a = MyStateT (s -> m (a, s))

type MyState s = MyStateT s Identity

instance (Functor m) => Functor (MyStateT s m) where
  fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
  fmap f (MyStateT sa) = MyStateT (fmap (first f) . sa)

instance (Monad m) => Applicative (MyStateT s m) where
  pure :: a -> MyStateT s m a
  pure a = MyStateT (\s -> pure (a, s))
  (<*>) :: forall a b . MyStateT s m (a -> b) -> MyStateT s m a -> MyStateT s m b
  MyStateT smab <*> MyStateT sma = MyStateT $ \s -> do
    (f, s') <- smab s
    (a, s'') <- sma s'
    return (f a, s'')

instance (Monad m) => Monad (MyStateT s m) where
  (>>=) :: MyStateT s m a -> (a -> MyStateT s m b) -> MyStateT s m b
  MyStateT sma >>= k = MyStateT $ \s -> do
    (a, s') <- sma s
    let MyStateT smb = k a
    smb s'

class (Monad m) => MyMonadState s m | m -> s where
  myGet :: m s
  myGet = myState (\s -> (s, s)) -- Don't affect the state, return the state as result.
  myPut :: s -> m ()
  myPut s = myState (const ((), s))
  myState :: (s -> (a, s)) -> m a
  myState f = do
    s <- myGet
    let (a, s') = f s
    myPut s'
    return a
  {-# MINIMAL myState | myGet, myPut #-}

instance (Monad m) => MyMonadState s (MyStateT s m) where
  myState :: (s -> (a, s)) -> MyStateT s m a
  myState f = MyStateT (return . f)

----------------------------------------------------

type Stack a = [a]

pop :: State (Stack a) a
pop = do
  xs <- get
  case xs of
    [] -> error "Can't pop an empty stack"
    x : xs' -> do
      put xs'
      return x

push :: a -> State (Stack a) ()
push x = modify (x :)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

threeCoinsTest :: IO ()
threeCoinsTest = do
  let coins = evalState threeCoins (mkStdGen 33)
  print coins
