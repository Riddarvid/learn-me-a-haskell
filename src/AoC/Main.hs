module AoC.Main where

import AoC.Utils.Input
import qualified AoC.Days.Day1 as Day1

main :: IO ()
main = do
  input <- parseInput "AoC/Input/input1"
  let result = Day1.solve input
  print $ show result
