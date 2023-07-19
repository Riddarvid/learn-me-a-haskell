{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use asum" #-}
{-# HLINT ignore "Use msum" #-}
{-# HLINT ignore "Use <&>" #-}
module MonadicFuns () where
import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (MonadPlus (mplus, mzero))

-- The reason that these functions exist is that they were useful before Applicative
-- was a prerequisite for Monad.

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = mx >>= return . f

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mf mx = mf >>= (\f -> mx >>= return . f)

-------------------------------------------

-- For convenience
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb

join' :: Monad m => m (m a) -> m a
join' mma = do
  ma <- mma
  ma

join'' :: Monad m => m (m a) -> m a
join'' mma = mma >>= (>>= return)

filterM' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = pure []
filterM' p (x : xs) = liftA2' (\b xs' -> if b then x : xs' else xs') fb fxs'
  where
    fb = p x
    fxs' = filterM' p xs

filterM'' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
filterM'' p = foldr (\x -> liftA2' (\b xs' -> if b then x : xs' else xs') (p x)) (pure [])

foldM' :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM' f b = foldl (\macc a -> macc >>= (`f` a)) (pure b)

-------------------------------------------------

asum' :: (Foldable t, Alternative f) => t (f a) -> f a
asum' = foldr (<|>) empty

msum' :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum' = foldr mplus mzero

mfilter' :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter' p ma = do
  a <- ma
  if p a
    then ma
    else mzero


