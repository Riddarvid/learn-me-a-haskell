{-# LANGUAGE InstanceSigs           #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Reader () where
import           Control.Monad.Identity (Identity)
import           Functors               (MyFunc (MyFunc))

newtype MyReaderT r m a = MyReaderT (r -> m a)

type MyReader r = MyReaderT r Identity

instance (Functor m) => Functor (MyReaderT r m) where
  fmap :: (a -> b) -> MyReaderT r m a -> MyReaderT r m b
  fmap f (MyReaderT ra) = MyReaderT (fmap f . ra)

instance (Applicative m) => Applicative (MyReaderT r m) where
  pure :: a -> MyReaderT r m a
  pure = MyReaderT . const . pure
  (<*>) :: MyReaderT r m (a -> b) -> MyReaderT r m a -> MyReaderT r m b
  MyReaderT rmab <*> MyReaderT rma = MyReaderT $ \r -> let
    f = rmab r
    x = rma r
    in f <*> x

instance (Monad m) => Monad (MyReaderT r m) where
  (>>=) :: MyReaderT r m a -> (a -> MyReaderT r m b) -> MyReaderT r m b
  MyReaderT rma >>= k = MyReaderT $ \r -> do
    a <- rma r
    let (MyReaderT rmb) = k a
    rmb r

-- Reader represents a computation reading from a shared environment.
-- ask can be used to read the environment.
-- local can be used to modify the local environment for a computation.
-- That is, before executing the computation, the function f should be applied to
-- the environment. This change is not "saved".
-- reader embeds a simple reader action.
class (Monad m) => MyMonadReader r m | m -> r where
  ask :: m r
  ask = reader id
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  reader f = f <$> ask

instance (Monad m) => MyMonadReader r (MyReaderT r m) where
  ask :: MyReaderT r m r
  ask = MyReaderT return
  local :: (r -> r) -> MyReaderT r m a -> MyReaderT r m a
  local envF (MyReaderT rf) = MyReaderT (rf . envF)

instance MyMonadReader r (MyFunc r) where
  ask :: MyFunc r r
  ask = MyFunc id
  local :: (r -> r) -> MyFunc r a -> MyFunc r a
  local envF (MyFunc f) = MyFunc (f . envF)
  reader :: (r -> a) -> MyFunc r a
  reader = MyFunc
