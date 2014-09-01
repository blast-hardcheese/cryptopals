module S01C01 where

import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.List as L
import Data.Maybe
import Control.Lens
import Data.Bits

import qualified Data.Char as C

-- utilities

w8toInt :: W.Word8 -> Int
w8toInt = fromIntegral

w8tow16 :: W.Word8 -> W.Word16
w8tow16 = fromIntegral

w16tow8 :: W.Word16 -> W.Word8
w16tow8 = fromIntegral

pairs :: String -> [(Char, Char)]  -- Drops incomplete pairs. "abc" == [('a', 'b')]
pairs (c1 : c2 : s) = (c1, c2) : pairs s
pairs _ = []

hex :: Int -> Maybe Char
hex b = "0123456789ABCDEF" ^? ix b

unhex :: Char -> Maybe Int
unhex c = L.findIndex (== C.toUpper c) ord
         where ord = "0123456789ABCDEF"

b64 :: Int -> Maybe Char
b64 b = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ^? ix b

-- hex string to byte string
hexToByteString :: String -> BS.ByteString
hexToByteString = BS.pack . (fmap fromIntegral) . unpackHexPairList . pairs

unpackHexPairList :: [(Char, Char)] -> [Int]
unpackHexPairList = catMaybes . (fmap unpackHexPair)

unpackHexPair :: (Char, Char) -> Maybe Int
unpackHexPair (c1, c2) = do x <- unhex c1
                            y <- unhex c2
                            return $ x*16 + y

-- byte string to hex string
bytestringToHex :: BS.ByteString -> [Char]
bytestringToHex bs = concat $ catMaybes $ fmap hexpair l
                   where l = bytestringToIntList bs

bytestringToIntList :: BS.ByteString -> [Int]
bytestringToIntList bs = BS.foldr (\b a -> (fromIntegral $ toInteger b) : a) [] bs

hexpair :: Int -> Maybe [Char]
hexpair i = do x <- hex (i `div` 16)
               y <- hex (i `mod` 16)
               Just [x, y]

-- base64 stuff
type B64Buff = (W.Word16, Int)

chunkByteString :: BS.ByteString -> [Int]
chunkByteString bs = fmap w8toInt $ reverse $ concat words
                     where (wholewords, newbuff) = BS.foldl chunkFold ([], (0, 0)) bs
                           words = wholewords

chunkFold :: ([[W.Word8]], B64Buff) -> W.Word8 -> ([[W.Word8]], B64Buff)
chunkFold (a, buff) n = (words : a, newbuff)
                        where (words, newbuff) = processBuffer n buff

flushBuffer :: B64Buff -> [W.Word8]
flushBuffer (bits, size) = fst $ doBuffer (bits, 6)

doBuffer :: B64Buff -> ([W.Word8], B64Buff)
doBuffer b@(bits, size)
    | size < 6 = ([], b)
    | otherwise = (extracted : nextbits, nextbuff)
                  where mask = (shift 1 6) - 1 :: W.Word16
                        shiftedmask = (shift mask (size - 6))
                        masked = shiftedmask .&. bits
                        extracted = w16tow8 $ shift masked (-size + 6)
                        newBuff = (complement shiftedmask) .&. bits
                        (nextbits, nextbuff) = doBuffer (newBuff, size - 6)

processBuffer :: W.Word8 -> B64Buff -> ([W.Word8], B64Buff)
processBuffer in8 buff = (reverse r, b)
         where wordsize = 8
               in16 = w8tow16 in8
               buffer = in16 .|. (shift (fst buff) wordsize)
               buffsize = wordsize + snd buff
               (r, b) = doBuffer (buffer, buffsize)

base64 :: String -> String
base64 = catMaybes . (fmap b64) . chunkByteString . hexToByteString
