module AoC.Days.Day2
    (solve
    ) where

import qualified Data.Text

data Policy = Policy Int Int Char String deriving (Show)

parsePassword :: String -> Policy
parsePassword input = Policy (read minString) (read maxString) character password
  where
    [limits, character:_, password] = words input
    (minString, _:maxString) = span (/='-') limits

solve :: [String] -> (Int, Int)
solve input = (part1 policies, part2 policies)
  where
    policies = map parsePassword input

part1 :: [Policy] -> Int
part1 policies = length $ filter isValid1 policies

part2 :: [Policy] -> Int
part2 policies = length $ filter isValid2 policies

isValid1 :: Policy -> Bool
isValid1 (Policy minVal maxVal c password) = count >= minVal && count <= maxVal
  where
    count = length [current | current <- password, current == c]

isValid2 :: Policy -> Bool
isValid2 (Policy minVal maxVal c password) = (password !! (minVal - 1) == c) /= (password !! (maxVal - 1) == c)
