{-# LANGUAGE PackageImports #-}
module S01C03 where

import Debug.Trace

import Data.Bits (xor)
import qualified Data.HashMap as HM
-- import qualified "hashmap" Data.HashSet as HS
import Data.Word (Word8)
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromMaybe, listToMaybe)
import Data.Ord (comparing)

import S01C01 (hexToByteString)
import qualified S01C02

import Utils (c2w8, i2f, w8toChar, s2ByteString)

-- Data

englishThreshold = 50 :: Float

type LanguageStats = HM.Map Word8 Float
englishStats :: LanguageStats
englishStats = HM.fromList [
        (10,1.7328233e-2),
        (13,1.7328233e-2),
        (32,0.17469306),
        (33,1.3194085e-4),
        (34,2.7673747e-3),
        (35,3.3830988e-6),
        (36,6.7661977e-6),
        (37,3.3830988e-6),
        (38,6.7661977e-6),
        (39,1.4141352e-3),
        (40,2.571155e-4),
        (41,2.571155e-4),
        (42,1.18408454e-4),
        (43,3.7214086e-5),
        (44,1.2625724e-2),
        (45,3.9616087e-3),
        (46,6.722217e-3),
        (47,1.18408454e-4),
        (48,1.9621973e-4),
        (49,1.028462e-3),
        (50,3.281606e-4),
        (51,2.706479e-4),
        (52,1.8268733e-4),
        (53,1.6915494e-4),
        (54,2.2666762e-4),
        (55,2.2328451e-4),
        (56,5.277634e-4),
        (57,1.6915494e-4),
        (58,4.939324e-4),
        (59,1.407369e-3),
        (63,1.5562255e-4),
        (64,3.3830988e-6),
        (91,2.334338e-4),
        (93,2.334338e-4),
        (95,8.9313806e-4),
        (97,6.1772e-2),
        (98,1.0386113e-2),
        (99,2.1983376e-2),
        (100,2.9602114e-2),
        (101,9.308258e-2),
        (102,2.0126054e-2),
        (103,1.4317274e-2),
        (104,4.5140687e-2),
        (105,5.5486202e-2),
        (106,1.370155e-3),
        (107,4.1848933e-3),
        (108,2.8539822e-2),
        (109,1.8498784e-2),
        (110,5.206589e-2),
        (111,5.597337e-2),
        (112,1.3928218e-2),
        (113,7.341324e-4),
        (114,4.7370147e-2),
        (115,4.890269e-2),
        (116,6.96986e-2),
        (117,2.0454215e-2),
        (118,7.398837e-3),
        (119,1.6881663e-2),
        (120,1.3430902e-3),
        (121,1.5629916e-2),
        (122,1.4547324e-4),
        (124,4.6010144e-4)
    ]

-- Apply single-byte xor
applySingleByte :: Word8 -> BS.ByteString -> BS.ByteString
applySingleByte b bs = BS.map (xor b) bs

analyze :: BS.ByteString -> Maybe Word8
analyze bs = listToMaybe $ fmap fst $ L.filter ((<englishThreshold) . snd) $ L.sortBy (comparing snd) allBytes
    where allBytes = [(x, determineProbability $ applySingleByte x bs) | x <- [0..255]]

determineProbability :: BS.ByteString -> Float
determineProbability = sum . (fmap snd) . determineProbabilities

determineProbabilities :: BS.ByteString -> [(Char, Float)]
determineProbabilities bs = fmap (\(c, v) -> (w8toChar c, v)) $ HM.toList $ HM.mapWithKey (\k v -> newVal v (fromMaybe 0.0001 $ HM.lookup k langMap)) freqMap
    where freqMap = buildFreqMap $ BS.map (c2w8 . toLower . w8toChar) bs
          langMap = filterFreqMap englishStats freqMap
          newVal v1 v2 = (abs $ v2 - v1) / v2

buildFreqMap :: BS.ByteString -> LanguageStats
buildFreqMap bs = HM.map countToFreq letterCount
    where inc = HM.alter $ Just . ((fromMaybe 1) . (fmap (+1))) :: Word8 -> HM.Map Word8 Int -> HM.Map Word8 Int
          letterCount = BS.foldr inc (HM.empty :: HM.Map Word8 Int) bs :: HM.Map Word8 Int
          bsLength = fromIntegral $ BS.length bs :: Float
          countToFreq = (/ bsLength) . fromIntegral :: Int -> Float

filterFreqMap :: LanguageStats -> LanguageStats -> LanguageStats
filterFreqMap language dataset = HM.map (/ total) filtered
    where filtered = HM.filterWithKey (\k _ -> HM.member k dataset) language
          total = sum $ fmap snd $ HM.toList filtered
