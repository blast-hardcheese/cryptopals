{-# LANGUAGE PackageImports #-}
module S01C03 where

import Data.Bits
import qualified Data.HashMap as HM
import qualified "hashmap" Data.HashSet as HS
import qualified Data.Word as W
import qualified Data.ByteString as BS
import qualified Data.Char as C

import qualified S01C02

-- Utility functions
c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . C.ord

w8toChar :: W.Word8 -> Char
w8toChar = C.chr . fromIntegral

s2ByteString :: String -> BS.ByteString
s2ByteString = BS.pack . (fmap c2w8)

-- Apply single-byte xor
applySingleByte :: W.Word8 -> BS.ByteString -> BS.ByteString
applySingleByte b bs = BS.map (xor b) bs

-- Frequency analysis
buildLetterMap :: BS.ByteString -> HM.Map Char Int
buildLetterMap bs = BS.foldr (updateMap . w8toChar) HM.empty bs
    where updateMap = \k m -> HM.insertWith (\_ v -> v + 1) (C.toLower k) 1 m

filterLetters :: HM.Map Char Int -> HM.Map Char Int
filterLetters = HM.filterWithKey isLetterKV
    where letterSet = HS.fromList "abcdefghijklmnopqrstuvwxyz"
          isLetter = (flip HS.member) letterSet
          isLetterKV = \k _ -> isLetter k

buildFilteredLetterMap :: BS.ByteString -> HM.Map Char Int
buildFilteredLetterMap = filterLetters . buildLetterMap

-- Process xor stuff

analyze :: String -> W.Word8
analyze _ = 0
