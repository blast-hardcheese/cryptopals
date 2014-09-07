{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module S01C06 where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits (xor, shift, shiftR, (.&.))

import Utils

xorByteStrings :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorByteStrings bs1 bs2 = BS.pack $ (fmap (uncurry xor)) $ BS.zip bs1 bs2

findBits :: Word8 -> Int
findBits b = fromIntegral $ foldr (\x a -> a + shiftR (b .&. (shift 1 x)) x) 0 [0..7]

class Hamming a where
    hamming :: a -> a -> Int

instance Hamming String where
    hamming s1 s2 = hamming (s2ByteString s1) (s2ByteString s2)

instance Hamming BS.ByteString where
    hamming bs1 bs2 = sum $ fmap findBits $ BS.unpack $ xorByteStrings bs1 bs2
