module S01C03Spec (main, spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import qualified Data.List as L

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

    it "should build letter maps from input" $ do
      let letters = s2ByteString "aabccc"
      buildLetterMap letters `shouldBe` HM.fromList [('a', 2), ('b', 1), ('c', 3)]

    it "should strip non-letter characters" $ do
      let hm = buildFilteredLetterMap $ s2ByteString "This is a test."
      let letters = "thisae"

      (L.sort $ HM.keys hm) `shouldBe` (L.sort letters)

    it "should find cypher byte" $ do
      let ct = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      analyze ct `shouldBe` 5
