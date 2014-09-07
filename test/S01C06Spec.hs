module S01C06Spec where

import Test.Hspec

import S01C06

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Hamming distance" $ do
        it "should correctly calculate the distance between two strings" $ do
            let fromstr = "this is a test"
            let tostr = "wokka wokka!!!"

            hamming fromstr tostr `shouldBe` 37
