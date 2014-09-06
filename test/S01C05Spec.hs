{-# LANGUAGE  QuasiQuotes #-}
module S01C05Spec where

import Test.Hspec

import Heredoc
import Utils

import qualified Data.List as L

import S01C01 (hexToByteString, bytestringToHex)
import S01C05

import qualified Data.ByteString as BS

sampleData = [str|Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal|]

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "Multi-byte xor" $ do
        it "should encrypt sample data" $ do
            let key = BS.unpack $ s2ByteString "ICE"
            let bss = s2ByteString sampleData

            (toLower $ bytestringToHex $ (encrypt key) bss) `shouldBe` (toLower $ L.foldr (++) "" [
                    "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272",
                    "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
                ])
