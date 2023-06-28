{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use asum" #-}
{-# HLINT ignore "Use msum" #-}
module MonadicFuns () where
import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (MonadPlus (mplus, mzero))

join' :: Monad m => m (m a) -> m a
join' mma = do
  ma <- mma
  ma

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


