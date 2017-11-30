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
import Data.Word

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

--  two functions come from cereal package innards (roll/unroll)
wordsToInteger :: [Word8] -> Integer
wordsToInteger = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

integerToWords :: Integer -> [Word8]
integerToWords = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

unsynchronise :: [Word8] -> Integer
unsynchronise = unsyncInteger . wordsToInteger

synchronise :: Integer -> [Word8]
synchronise = integerToWords . syncInteger
