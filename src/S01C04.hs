module S01C04 (main, testData, findLine) where

import qualified Data.Word as W
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes, mapMaybe)

import S01C01 (hexToByteString)
import S01C03 (applySingleByte, analyze)
import Utils (byteStringToString)

testData :: IO String
testData = readFile "data/4.txt"

findLine :: String -> [String]
findLine testfile = fmap (byteStringToString . (uncurry $ applySingleByte)) bytes
    where xs = fmap hexToByteString $ lines testfile
          bytes = mapMaybe (\x -> fmap (\b -> (b, x)) $ analyze x) xs

main :: IO()
main = testData >>= (\s -> putStrLn $ unlines $ findLine s)

-- main :: IO()
-- main = putStrLn $ show $ maybeAnalyze bs
--     where bs = hexToByteString "
--           shifted = applySingleByte 7 bs
