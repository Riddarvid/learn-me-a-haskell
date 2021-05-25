module AoC.Utils.Input
    (parseInput
    ) where

parseInput path = lines <$> readFile path
