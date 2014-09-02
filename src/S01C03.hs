module S01C03 where

import Data.Bits
import qualified Data.Word as W
import qualified Data.ByteString as BS

applySingleByte :: W.Word8 -> BS.ByteString -> BS.ByteString
applySingleByte b bs = BS.map (xor b) bs

analyze :: String -> W.Word8
analyze _ = 0
