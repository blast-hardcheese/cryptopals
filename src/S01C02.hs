module S01C02 where

import Data.Bits (xor)
import qualified Data.ByteString as BS

import S01C01 (hexToByteString, bytestringToHex)

xorByteStrings :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorByteStrings a b = BS.pack $ BS.zipWith xor a b

xorHexStrings :: String -> String -> String
xorHexStrings a b = bytestringToHex $ xorByteStrings bsa bsb
    where bsa = hexToByteString a
          bsb = hexToByteString b
