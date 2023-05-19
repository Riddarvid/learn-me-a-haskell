{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use getContents" #-}
module FileIO (
  myGetContents,
  printContents,
  printContents',
  myWithFile,
  myReadFile,
  myWriteFile,
  myAppendFile
) where
import           Control.Monad ((>=>))
import           System.IO     (Handle,
                                IOMode (AppendMode, ReadMode, WriteMode),
                                hClose, hGetContents, hPutStr, openFile, stdin,
                                withFile)

myGetContents :: IO String
myGetContents = hGetContents stdin

printContents :: FilePath -> IO ()
printContents path = do
  hdl <- openFile path ReadMode
  contents <- hGetContents hdl
  putStrLn contents
  hClose hdl

printContents' :: FilePath -> IO ()
printContents' path = withFile path ReadMode (hGetContents >=> putStrLn)

myWithFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
myWithFile path mode f = do
  hdl <- openFile path mode
  res <- f hdl
  hClose hdl
  return res

myReadFile :: FilePath -> IO String
myReadFile path = myWithFile path ReadMode hGetContents

myWriteFile :: FilePath -> String -> IO ()
myWriteFile path content = myWithFile path WriteMode (`hPutStr` content)

myAppendFile :: FilePath -> String -> IO ()
myAppendFile path content = myWithFile path AppendMode (`hPutStr` content)
