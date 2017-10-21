{-# LANGUAGE OverloadedStrings #-}

module ID3v1
    ( ID3v1
    , emptyID3v1
    ) where

import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C

import ID3v1.Genre
import Tag

data ID3v1
    = ID3v1 {
    title   :: C.ByteString,
    album   :: C.ByteString,
    year    :: C.ByteString,
    comment :: C.ByteString,
    track   :: Maybe Word8, -- ID3v1.1 adds track field
    genre   :: Word8
    } deriving Eq

emptyID3v1 :: ID3v1
emptyID3v1 = ID3v1 "" "" "" "" Nothing 255

instance Tag ID3v1 where
    version t =
        case (track t) of
            Just _ -> "ID3v1.1"
            Nothing -> "ID3v1"

instance Show ID3v1 where
    show t =
        ("Title: " ++ (show $ title t))               ++"\n"++
        ("Album: " ++ (show $ album t))               ++"\n"++
        ("Year: " ++ (show $ year t))                 ++"\n"++
        ("Comment: " ++ (show $ comment t))           ++
        (maybe "" (("\nTrack: " ++) . show) (track t))  ++
        (maybe "" (("\nGenre: " ++) . show) (toGenre . genre $ t))
