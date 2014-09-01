module S01C01Spec (main, spec) where

import Test.Hspec

import S01C01

main :: IO()
main = hspec spec

spec :: Spec
spec = do

  describe "base64" $ do
    it "should encode a hex string into base64" $ do
        let hexstring = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        base64 hexstring `shouldBe` Just "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
