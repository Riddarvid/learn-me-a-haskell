module AoC.Main where

import AoC.Utils.Input
import qualified AoC.Days.Day1 as Day1
import qualified AoC.Days.Day2 as Day2

main :: IO ()
main = do
  input <- parseInput "AoC/Input/input2"
  let result = Day2.solve input
  print result
