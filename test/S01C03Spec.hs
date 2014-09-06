module S01C03Spec (main, spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import qualified Data.List as L

import Utils (s2ByteString)
import S01C01 (hexToByteString, bytestringToHex)
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

  let ct88 = hexToByteString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let ct131 = applySingleByte 131 $ s2ByteString "I know not all that may be coming, but be it what it will, I'll go to it laughing."
  let bogus = s2ByteString "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\33\34\35\36\37\38\39\40\41\42\43\44\45\46\47\48\49\50\51\52\53\54\55\56\57\58\59\60\61\62\63\64\65\66\67\68\69\70\71\72\73\74\75\76\77\78\79\80\81\82\83\84\85\86\87\88\89\90\91\92\93\94\95\96\97\98\99\100\101\102\103\104\105\106\107\108\109\110\111\112\113\114\115\116\117\118\119\120\121\122\123\124\125\126\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255"

  describe "process" $ do
    it "should determine that there exists a byte that produces English text" $ do
      analyze ct131 `shouldBe` Just 131

    it "should determine that there is not a byte that produces English text" $ do
      analyze bogus `shouldBe` Nothing

    it "should take a ByteString and determine a ranking of it being a representation of English" $ do
      determineProbability (s2ByteString "ee") `shouldBe` 0

      determineProbability ct131 `shouldSatisfy` (> englishThreshold)
      determineProbability bogus `shouldSatisfy` (> englishThreshold)

    it "should find cypher byte" $ do
      analyze ct88 `shouldBe` Just 88
      analyze ct131 `shouldBe` Just 131
