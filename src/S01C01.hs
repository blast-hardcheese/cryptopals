module S01C01 where

import qualified Data.ByteString as BS
import qualified Data.Word as W
import Data.Word (Word8)
import qualified Data.List as L
import Data.Maybe
import Control.Lens
import Data.Bits
import Utils (w8toInt, w8tow16, w16tow8, pairs)

import qualified Data.Char as C

-- utilities

hex :: Int -> Maybe Char
hex b = "0123456789ABCDEF" ^? ix b

unhex :: Char -> Maybe Int
unhex c = L.findIndex (== C.toUpper c) ord
         where ord = "0123456789ABCDEF"

b64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
b64 :: Int -> Maybe Char
b64 b = b64chars ^? ix b

unb64 :: Char -> Maybe Int
unb64 c = L.findIndex (== C.toUpper c) b64chars

-- hex string to byte string
hexToByteString :: String -> BS.ByteString
hexToByteString = BS.pack . (fmap fromIntegral) . unpackHexPairList . pairs

unpackHexPairList :: [(Char, Char)] -> [Int]
unpackHexPairList = mapMaybe unpackHexPair

unpackHexPair :: (Char, Char) -> Maybe Int
unpackHexPair (c1, c2) = do x <- unhex c1
                            y <- unhex c2
                            return $ x*16 + y

-- byte string to hex string
bytestringToHex :: BS.ByteString -> [Char]
bytestringToHex bs = concat $ mapMaybe hexpair l
                   where l = bytestringToIntList bs

bytestringToIntList :: BS.ByteString -> [Int]
bytestringToIntList bs = BS.foldr (\b a -> (fromIntegral $ toInteger b) : a) [] bs

hexpair :: Int -> Maybe [Char]
hexpair i = do x <- hex (i `div` 16)
               y <- hex (i `mod` 16)
               Just [x, y]

-- base64 stuff
type B64Buff = (W.Word16, Int)

bufferToPartial :: B64Buff -> ([Word8], Int)
bufferToPartial (0, 0) = ([], 0)
bufferToPartial (buff, bs) = (fst $ doBuffer (shift buff offset, bs + offset), offset `div` 2)
    where offset = 6 - bs

chunkByteString :: BS.ByteString -> ([Int], Int)
chunkByteString bs = (fmap w8toInt $ reverse $ concat words, paddingBits)
                     where (wholewords, newbuff) = BS.foldl chunkFold ([], (0, 0)) bs
                           (padding, paddingBits) = bufferToPartial newbuff
                           words = [padding] ++ wholewords

chunkFold :: ([[W.Word8]], B64Buff) -> W.Word8 -> ([[W.Word8]], B64Buff)
chunkFold (a, buff) n = (words : a, newbuff)
                        where (words, newbuff) = processBuffer n buff

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
base64 hexString = (mapMaybe b64 bytes) ++ (take paddingBytes $ repeat '=')
    where (bytes, paddingBytes) = chunkByteString $ hexToByteString hexString

i2w8 :: Int -> Word8
i2w8 = fromIntegral

unchunkByteString :: [Int] -> BS.ByteString
unchunkByteString = fst . (foldr (step . i2w8) (BS.empty, (0, 0)))

step :: Word8 -> (BS.ByteString, B64Buff) -> (BS.ByteString, B64Buff)
step b (bs, buff) = flushDecBuffer (bs, insertBuff b buff)
    where insertBuff b (buff, bc) = ((shift (w8tow16 b) bc) .|. (shift buff 0), bc + 6)

flushDecBuffer :: (BS.ByteString, B64Buff) -> (BS.ByteString, B64Buff)
flushDecBuffer b@(bs, (buff, bc))
    | bc < 8 = b
    | otherwise = flushDecBuffer (newBS, (newBuff, bc - 8))
        where newBuff = shiftR buff 8
              consChar = buff `xor` (shift newBuff 8)
              newBS = BS.cons (w16tow8 consChar) bs

unbase64 :: String -> String
unbase64 = bytestringToHex . unchunkByteString . (mapMaybe unb64)
