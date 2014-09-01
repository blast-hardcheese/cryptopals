module S01C01Spec (main, spec) where

import Test.Hspec

import S01C01

import qualified Data.ByteString as BS

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

    it "should round-trip to/from ByteString" $ do
        (bytestringToHex $ hexToByteString "13A1") `shouldBe` "13A1"

  describe "base64" $ do
    it "should encode a hex string into base64" $ do
        let hexstring = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        base64 hexstring `shouldBe` Just "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
