{-# LANGUAGE OverloadedStrings #-}

module ID3v1_2
    ( ID3v1_2
    , emptyID3v1_2
    , id3v1_2
    ) where

import Prelude hiding (take)
import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import ID3v1.Genre
import Tag

data ID3v1_2
    = ID3v1_2 {
    title       :: C.ByteString,
    artist      :: C.ByteString,
    album       :: C.ByteString,
    speed       :: Word8,
    genre       :: C.ByteString,
    start_time  :: C.ByteString,
    end_time    :: C.ByteString
    } deriving Eq

emptyID3v1_2 :: ID3v1_2
emptyID3v1_2 = ID3v1_2 "" "" "" 255 "" "" ""

id3v1_2 :: Parser ID3v1_2
id3v1_2 = do
    string "TAG+"
    t <- take 60
    a <- take 60
    b <- take 30
    s <- anyWord8
    g <- take 30
    t1 <- take 6
    t2 <- take 6
    endOfInput
    return $ ID3v1_2 t a b s g t1 t2

instance Tag ID3v1_2 where
    version t = "ID3v1_2"

instance Show ID3v1_2 where
    show t =
        ("Title: " ++ (form $ title t))                ++"\n"++
        ("Artist: " ++ (form $ artist t))              ++"\n"++
        ("Album: " ++ (form $ album t))                ++"\n"++
        (("Speed: " ++) . show) (speed t) ++
        ("Genre: " ++ (form $ genre t))                ++"\n"++
        ("Start: " ++ (form $ start_time t))           ++"\n"++
        ("End: " ++ (form $ end_time t))
        where
            -- convert bytestring to initial printable ascii chars
            form = show . B.takeWhile (\c -> c >= 20 && c < 127)
