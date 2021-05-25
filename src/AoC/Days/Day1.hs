module AoC.Days.Day1
    (solve
    ) where

import qualified Data.Set as Set

solve :: [String] -> (Integer, String)
solve input = (part1 parsed, part2 parsed)
  where
    parsed = map read input

part1 :: [Integer] -> Integer
part1 input = x * y
  where
    (x, y) = head [(x, y) | x <- input, let y = 2020 - x, Set.member y inputSet]
    inputSet = Set.fromList input

part2 :: [Integer] -> String
part2 input = "Second result"

get2Terms target input = head [(x, y) | x <- input, let y = target - x, Set.member y inputSet]
where
  inputSet = Set.fromList input

getTerms :: Integer -> Integer -> [Integer] -> Maybe [Integer]
getTerms 0 _ _ = Nothing
getTerms 1 
