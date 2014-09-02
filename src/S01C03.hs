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

i2f :: Int -> Float
i2f = fromIntegral

w8toChar :: W.Word8 -> Char
w8toChar = C.chr . fromIntegral

s2ByteString :: String -> BS.ByteString
s2ByteString = BS.pack . (fmap c2w8)

s2lfm :: String -> HM.Map Char Float
s2lfm = buildLetterFreqMap . buildFilteredLetterMap . s2ByteString

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

buildLetterFreqMap :: HM.Map Char Int -> HM.Map Char Float
buildLetterFreqMap hm = HM.map (\x -> (i2f x) / numChars)  hm
    where numChars = i2f $ sum $ HM.elems hm

checkLetterFreqMap :: HM.Map Char Float -> (Char -> Float) -> Float
checkLetterFreqMap hm f = HM.foldWithKey (\k v a -> ((abs $ v - (f k)) / (f k)) + a) 0 hm

englishLetterFreqs :: Char -> Float
englishLetterFreqs l = case l of
    'a' -> 8.167
    'b' -> 1.492
    'c' -> 2.782
    'd' -> 4.253
    'e' -> 12.702
    'f' -> 2.228
    'g' -> 2.015
    'h' -> 6.094
    'i' -> 6.966
    'j' -> 0.153
    'k' -> 0.772
    'l' -> 4.025
    'm' -> 2.406
    'n' -> 6.749
    'o' -> 7.507
    'p' -> 1.929
    'q' -> 0.095
    'r' -> 5.987
    's' -> 6.327
    't' -> 9.056
    'u' -> 2.758
    'v' -> 0.978
    'w' -> 2.360
    'x' -> 0.150
    'y' -> 1.974
    'z' -> 0.074

-- Process xor stuff

analyze :: String -> W.Word8
analyze _ = 0
