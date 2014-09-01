module S01C01 where

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe

hexToByteString :: String -> BS.ByteString
hexToByteString = BS.pack . (fmap fromIntegral) . unpackHexPairList . pairs

unpackHexPairList :: [(Char, Char)] -> [Int]
unpackHexPairList = catMaybes . (fmap unpackHexPair)

unpackHexPair :: (Char, Char) -> Maybe Int
unpackHexPair (c1, c2) = (unhex c1) >>= (\x -> (unhex c2) >>= (\y -> Just $ x * 16 + y))

unhex :: Char -> Maybe Int
unhex c = L.findIndex (== c) ord
         where ord = "0123456789abcdef"

pairs :: String -> [(Char, Char)]  -- Drops incomplete pairs. "abc" == [('a', 'b')]
pairs (c1 : c2 : s) = (c1, c2) : pairs s
pairs _ = []

base64 :: String -> Maybe String
base64 _ = Nothing
