module ByteStrings (
  exBS,
  exBL,
  consBS,
  consBL,
  cons'BL
) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word8)

exBytes :: [Word8]
exBytes = [104, 101, 108, 108, 111]

exBS :: BS.ByteString
exBS = BS.pack exBytes

exBL :: BL.ByteString
exBL = BL.pack exBytes

consBS :: BS.ByteString
consBS = BS.cons 33 exBS

consBL :: BL.ByteString
consBL = BL.cons 33 exBL

cons'BL :: BL.ByteString
cons'BL = BL.cons' 33 exBL
