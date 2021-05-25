module AoC.Days.Day1
    (solve
    ) where

import qualified Data.Set as Set

solve :: [String] -> (Integer, Integer)
solve input = (part1 parsed, part2 parsed)
  where
    parsed = map read input

part1 :: [Integer] -> Integer
part1 input = product $ getTerms 2 2020 input

part2 :: [Integer] -> Integer
part2 input = product $ getTerms 3 2020 input

getTerms :: Integer -> Integer -> [Integer] -> [Integer]
getTerms 0 _ _ = []
getTerms _ _ [] = []
getTerms 1 target input = [target | target `elem` input]
getTerms n target (x:xs) = if not $ null tailTerms
  then x:tailTerms
  else getTerms n target xs
  where
    tailTerms = getTerms (n - 1) (target - x) xs
