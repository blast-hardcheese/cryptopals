module S01C02Spec (main, spec) where

import Test.Hspec

import S01C02

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "xor" $ do
    it "should xor hex strings" $ do
      let cs1 = "1c0111001f010100061a024b53535009181c"
      let cs2 = "686974207468652062756c6c277320657965"
      let target = "746865206B696420646F6E277420706C6179"
      xorHexStrings cs1 cs2 `shouldBe` target
