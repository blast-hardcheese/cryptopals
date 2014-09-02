module S01C03Spec (main, spec) where

import Test.Hspec

import S01C03

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "single-byte xor" $ do
    it "should find cypher byte" $ do
      let ct = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      analyze ct `shouldBe` 5
