{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module InputOutput (
  helloWorld,
  reverseWordsLoop,
  myPrint,
  getContentsTest
) where

helloWorld :: IO ()
helloWorld = putStrLn "Hello, world!"

--------------------------

-- The functions reverseWordsLoop and reverseWordsLoop' behave in the same way, although they don't do
-- exactly the same thing internally.

reverseWordsLoop :: IO ()
reverseWordsLoop = do
  line <- getLine
  if null line
    then return ()
    else do
      let reversed = reverseWords line
      putStrLn reversed
      reverseWordsLoop

reverseWordsLoop' :: IO ()
reverseWordsLoop' = interact $ unlines . map reverseWords . lines

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

------------------------------

myPrint :: (Show a) => a -> IO ()
myPrint = putStrLn . show

-----------------------------------------

-- >>
bindM :: (Monad m) => m a -> m b -> m b
--bind' actionA actionB = actionA >>= (\_ -> actionB)
bindM = bindA

-- *>
bindA :: (Applicative f) => f a -> f b -> f b
bindA actionA actionB = (\_ x -> x) <$> actionA <*> actionB

-----------------------------------------

myForever :: (Applicative f) => f a -> f b
myForever action = action *> myForever action

myInteract :: (String -> String) -> IO ()
myInteract f = getContents >>= (putStr . f)

-- putStr will start printing as soon as it has even a single character of it's string input. getContents only returns
-- characters once the user presses enter. Therefore, the partial "response" is printed to the screen as soon as
-- the user presses enter, giving the illusion of a program/function being run many times.
getContentsTest :: IO ()
getContentsTest = getContents >>= putStr
