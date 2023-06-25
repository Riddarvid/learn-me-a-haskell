module Alternative (altMaybeTest) where
import           Control.Applicative (Alternative (empty, many, some, (<|>)))

altMaybeTest :: IO ()
altMaybeTest = do
  let j1 = Just 5 :: Maybe Int
  let j2 = Just 7 :: Maybe Int
  let j3 = Nothing :: Maybe Int
  putStr "Empty: "
  print (empty :: Maybe Int)
  putStrLn ""
  putStrLn "<|>: "
  print (j1 <|> j2)
  print (j2 <|> j1)
  print (j1 <|> j3)
  print (j3 <|> j1)
  print (j2 <|> j3)
  print (j3 <|> j2)
  putStrLn ""
  putStrLn "some"
  print $ head <$> many j3
  --print $ some j2
  --print $ some j3
