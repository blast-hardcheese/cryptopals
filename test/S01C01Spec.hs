module S01C01Spec (main, spec) where

import Test.Hspec

import S01C01

import qualified Data.ByteString as BS
import qualified Data.Bits as Bits

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "base64 helpers" $ do
    it "should decode a base64 Char" $ do
        unhex 'a' `shouldBe` Just 10

    it "should decode a base64 pair" $ do
        unpackHexPair ('1', '3') `shouldBe` Just 19

    it "should decode a list of base64 pairs" $ do
        unpackHexPairList [('1', '3'), ('a', '1')] `shouldBe` [19, 161]

    it "should decode a base64 string" $ do
        hexToByteString "13a1" `shouldBe` BS.pack [19, 161]

    it "should encode a ByteString to an int list" $ do
        bytestringToIntList (BS.pack [19, 161]) `shouldBe` [19, 161]

    it "should convert an integer between 0 and 15 into a single hex character" $ do
        hex 13 `shouldBe` Just 'D'

    it "should encode a ByteString to hex" $ do
        bytestringToHex (BS.pack [19, 161]) `shouldBe` "13A1"

    it "should encode a ByteString to hex preserving leading zeroes" $ do
        bytestringToHex (BS.pack [5, 5]) `shouldBe` "0505"

    it "should round-trip to/from ByteString" $ do
        (bytestringToHex $ hexToByteString "13A1") `shouldBe` "13A1"

  describe "base64" $ do
    it "should chunk new input with existing buffer" $ do
        let buildBits n = (Bits.shift 1 n) - 1
        let fourBits = buildBits 4
        let sixBits = buildBits 6
        let eightBits = buildBits 8
        processBuffer eightBits (fourBits, 4) `shouldBe` ([sixBits, sixBits], (0, 0))

    it "should chunk a ByteString into 6-bit Word8s" $ do
        chunkByteString (hexToByteString "13A1") `shouldBe` ([4, 58, 4], 1)

    it "should encode a hex string into base64" $ do
        let hexstring = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        base64 hexstring `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    it "should chunk partial byte strings" $ do
        (chunkByteString $ BS.pack [0]) `shouldBe` ([0, 0], 2)

    it "should appropriately pad" $ do
        base64 "00" `shouldBe` "AA=="

  describe "unbase64" $ do
    it "should step" $ do
      step 0 (BS.empty, (0, 0)) `shouldBe` (BS.empty, (0, 6))
      step 1 (BS.empty, (0, 0)) `shouldBe` (BS.empty, (1, 6))

      let first = step 1 (BS.empty, (0, 0))
      first `shouldBe` (BS.empty, (1, 6))

      let second = step 0 first
      second `shouldBe` (BS.pack [1], (0, 4))

      let third = step 1 second
      third `shouldBe` (BS.pack [16, 1], (0, 2))

    it "should unchunk bytestring" $ do
      unchunkByteString [0, 1, 0, 1] `shouldBe` BS.pack [0, 16, 1]

    it "should decode 32 bytes" $ do
      unbase64 "ABAB" `shouldBe` "001001"
