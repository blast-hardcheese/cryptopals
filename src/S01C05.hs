module S01C05 where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits (xor)

encrypt :: [Word8] -> BS.ByteString -> BS.ByteString
encrypt bytes bs = BS.pack $ fmap (uncurry xor) pairs
    where pairs = zip (cycle bytes) $ BS.unpack bs
