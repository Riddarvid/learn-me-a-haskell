module Random (
  randomExample,
  randoms',
  finiteRandoms
) where
import           System.Random (Random (random), RandomGen, mkStdGen)


randomExample :: Random a => a
randomExample = fst $ random $ mkStdGen 100

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (val, gen') = random gen in val : randoms' gen'

finiteRandoms :: (RandomGen g, Random a, Integral n) => n -> g -> ([a], g)
finiteRandoms n gen
  | n <= 0 = ([], gen)
  | otherwise = (val : vals, lastGen)
  where
    (val, gen') = random gen
    (vals, lastGen) = finiteRandoms (n - 1) gen'
