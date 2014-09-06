module Utils where

import qualified Data.Word as W

w8toInt :: W.Word8 -> Int
w8toInt = fromIntegral

w8tow16 :: W.Word8 -> W.Word16
w8tow16 = fromIntegral

w16tow8 :: W.Word16 -> W.Word8
w16tow8 = fromIntegral

pairs :: String -> [(Char, Char)]  -- Drops incomplete pairs. "abc" == [('a', 'b')]
pairs (c1 : c2 : s) = (c1, c2) : pairs s
pairs _ = []
