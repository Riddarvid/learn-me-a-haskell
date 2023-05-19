{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use getContents" #-}
module FileIO (myGetContents) where
import           System.IO (hGetContents, stdin)

myGetContents :: IO String
myGetContents = hGetContents stdin
