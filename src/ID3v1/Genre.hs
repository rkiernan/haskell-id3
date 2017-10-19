module ID3v1.Genre
    ( toGenre
    , fromGenre
    ) where

import Data.Word (Word8)
import qualified Data.ByteString.Char8 as C

toGenre :: Word8 -> String
toGenre 0   = "Blues"
toGenre 1   = "Classic Rock"
-- ....... for now
toGenre 147 = "Synthpop"
toGenre _   = "N/A"

fromGenre :: String -> Word8
fromGenre "Blues"        = 0
fromGenre "Classic Rock" = 1
-- ....... for now
fromGenre "Synthpop"     = 147
fromGenre _              = 255
