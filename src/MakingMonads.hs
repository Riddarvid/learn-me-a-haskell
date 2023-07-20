{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module MakingMonads () where
import           Control.Monad  (ap)
import           Data.Bifunctor (Bifunctor (second), first)
import qualified Data.Map       as Map

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

instance Functor Prob where
  fmap :: (a -> b) -> Prob a -> Prob b
  fmap f (Prob xs) = Prob $ map (first f) xs

instance Applicative Prob where
  pure :: a -> Prob a
  pure x = Prob [(x, 1)]
  (<*>) :: Prob (a -> b) -> Prob a -> Prob b
  (<*>) = ap

instance Monad Prob where
  (>>=) :: Prob a -> (a -> Prob b) -> Prob b
  Prob probs >>= k = Prob $
    concatMap (\(a, pa) -> let Prob probs' = k a in map (second (pa *)) probs') probs

flattenProbs :: Ord a => Prob a -> Prob a
flattenProbs (Prob ps) = Prob $ Map.toList $ foldr (uncurry (Map.insertWith (+))) Map.empty ps
