module S01C01 where

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe
import Control.Lens

import qualified Data.Char as C

hexToByteString :: String -> BS.ByteString
hexToByteString = BS.pack . (fmap fromIntegral) . unpackHexPairList . pairs

unpackHexPairList :: [(Char, Char)] -> [Int]
unpackHexPairList = catMaybes . (fmap unpackHexPair)

unpackHexPair :: (Char, Char) -> Maybe Int
unpackHexPair (c1, c2) = do x <- unhex c1
                            y <- unhex c2
                            return $ x*16 + y

unhex :: Char -> Maybe Int
unhex c = L.findIndex (== C.toUpper c) ord
         where ord = "0123456789ABCDEF"

pairs :: String -> [(Char, Char)]  -- Drops incomplete pairs. "abc" == [('a', 'b')]
pairs (c1 : c2 : s) = (c1, c2) : pairs s
pairs _ = []

bytestringToHex :: BS.ByteString -> [Char]
bytestringToHex bs = catMaybes $ concat $ fmap bighex l
                   where l = bytestringToIntList bs

bytestringToIntList :: BS.ByteString -> [Int]
bytestringToIntList bs = BS.foldr (\b a -> (fromIntegral $ toInteger b) : a) [] bs

bighexiter :: Int -> [Maybe Char]
bighexiter 0 = []
bighexiter num = (hex $ num `mod` 16) : bighex (num `div` 16)

bighex :: Int -> [Maybe Char]
bighex = reverse . bighexiter

hex :: Int -> Maybe Char
hex b = "0123456789ABCDEF" ^? ix b

base64 :: String -> Maybe String
base64 _ = Nothing
