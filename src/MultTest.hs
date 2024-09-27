{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NumericUnderscores #-}
module MultTest (multTest) where
import           Control.DeepSeq   (NFData, force)
import           Control.Exception (evaluate)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (fromJust)
import           Data.Time         (UTCTime, diffUTCTime, getCurrentTime,
                                    nominalDiffTimeToSeconds)
import           GHC.Utils.Misc    (chunkList)
import           System.Random     (Random (randomRs), getStdGen)

type MultType = Int

multiplyNormal :: MultType -> MultType -> MultType
multiplyNormal = (*)

multMap :: HashMap (MultType, MultType) MultType
multMap = HM.fromList [((x, y), x * y) | x <- [0..99], y <- [0..99]]

multiplyLookup :: MultType -> MultType -> MultType
multiplyLookup x y = fromJust $ HM.lookup (x, y) multMap

multMany :: (MultType -> MultType -> MultType) -> [(MultType, MultType)] -> [MultType]
multMany = map . uncurry

generateDigitPairs :: IO [(MultType, MultType)]
generateDigitPairs = do
  g <- getStdGen
  let digitStream = randomRs (0, 99) g :: [Int]
  let chunks = chunkList 2 digitStream
  return $ map (\[a, b] -> (a, b)) chunks

multTest :: IO ()
multTest = do
  pairs <- take 10_000_000 <$> generateDigitPairs
  (_, timeNormal) <- evalTimed $ multMany multiplyNormal pairs
  putStrLn $ "Normal: " ++ show timeNormal ++ "s"
  (_, timeLookup) <- evalTimed $ multMany multiplyLookup pairs
  putStrLn $ "Lookup: " ++ show timeLookup ++ "s"

evalTimed :: NFData a => a -> IO (a, Double)
evalTimed x = do
  start <- getCurrentTime
  x' <- evaluate $ force x
  end <- getCurrentTime
  return (x', sBetween end start)

sBetween :: UTCTime -> UTCTime -> Double
sBetween end start =
  realToFrac $ nominalDiffTimeToSeconds (diffUTCTime end start)
