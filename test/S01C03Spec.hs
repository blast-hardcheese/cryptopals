module S01C03Spec (main, spec) where

import Test.Hspec
import qualified Data.ByteString as BS

import S01C03

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "single-byte xor" $ do
    it "should apply single byte to ByteString" $ do
      let bs = BS.pack [1,2,3,4,5,6]
      let target = BS.pack [0, 3, 2, 5, 4, 7]

      applySingleByte 0 bs `shouldBe` bs
      applySingleByte 1 bs `shouldBe` target

    it "should find cypher byte" $ do
      let ct = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      analyze ct `shouldBe` 5
