module Utils where

import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.Char as C

-- Utility functions
c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . C.ord

w8toChar :: W.Word8 -> Char
w8toChar = C.chr . fromIntegral

i2f :: Int -> Float
i2f = fromIntegral

w8toInt :: W.Word8 -> Int
w8toInt = fromIntegral

w8tow16 :: W.Word8 -> W.Word16
w8tow16 = fromIntegral

w16tow8 :: W.Word16 -> W.Word8
w16tow8 = fromIntegral

s2ByteString :: String -> BS.ByteString
s2ByteString = BS.pack . (fmap c2w8)

pairs :: String -> [(Char, Char)]  -- Drops incomplete pairs. "abc" == [('a', 'b')]
pairs (c1 : c2 : s) = (c1, c2) : pairs s
pairs _ = []

byteStringToString :: BS.ByteString -> String
byteStringToString = (fmap w8toChar) . BS.unpack

toLower :: String -> String
toLower = fmap C.toLower
