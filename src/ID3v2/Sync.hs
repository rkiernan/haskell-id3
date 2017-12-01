module ID3v2.Sync
    ( unsyncInteger
    , syncInteger
    , wordsToInteger
    , integerToWords
    , unsynchronise
    , synchronise
    ) where

import Data.List
import Data.Bits
import Data.Word (Word8)

unsyncInteger :: Integer -> Integer
unsyncInteger n = 
    (n .&. 0x7f)
    .|.  (n .&. 0x7f00)     `shiftR` 1
    .|.  (n .&. 0x7f0000)   `shiftR` 2
    .|.  (n .&. 0x7f000000) `shiftR` 3

syncInteger :: Integer -> Integer
syncInteger n = 
    (n .&. 0x7f)
    .|. (n .&. 0x3f80)    `shiftL` 1
    .|. (n .&. 0x1fc000)  `shiftL` 2
    .|. (n .&. 0xfe00000) `shiftL` 3

-- function comes from cereal package innards
wordsToInteger :: [Word8] -> Integer
wordsToInteger = foldr unstep 0 . reverse
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

-- convert n to [Word8] in BE of length s
integerToWords :: Int -> Integer -> [Word8]
integerToWords s n = map (fromInteger . ($ n) . f) $ reverse [0..(s-1)]
  where
    f l = \n -> (n `shiftR` (l*8)) .&. 0xff

unsynchronise :: [Word8] -> Integer
unsynchronise = unsyncInteger . wordsToInteger

synchronise :: Int -> Integer -> [Word8]
synchronise s = integerToWords s . syncInteger
