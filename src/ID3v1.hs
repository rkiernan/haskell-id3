{-# LANGUAGE OverloadedStrings #-}

module ID3v1
    ( ID3v1
    , emptyID3v1
    , id3v1
    ) where

import Prelude hiding (take)
import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import ID3v1.Genre
import Tag

data ID3v1
    = ID3v1 {
    title   :: C.ByteString,
    artist  :: C.ByteString,
    album   :: C.ByteString,
    year    :: C.ByteString,
    comment :: C.ByteString,
    track   :: Maybe Word8, -- ID3v1.1 adds track field
    genre   :: Word8
    } deriving Eq
--TODO add support for ID3v1.2 which adds extended tag

emptyID3v1 :: ID3v1
emptyID3v1 = ID3v1 "" "" "" "" "" Nothing 255

id3v1 :: Parser ID3v1
id3v1 = do
    string "TAG"
    t <- take 30
    a <- take 30
    b <- take 30
    y <- take 4
    c <- take 28
    t1 <- anyWord8
    t2 <- anyWord8
    g <- anyWord8
    endOfInput
    let notrack  = ID3v1 t a b y (B.append c (B.pack [t1, t2])) Nothing g
    let yestrack = ID3v1 t a b y c (Just t2) g
    return $ case (t1,t2) of
                (0,0) -> notrack
                (0,_) -> yestrack
                (_,_) -> notrack

instance Tag ID3v1 where
    version t =
        case (track t) of
            Just _ -> "ID3v1.1"
            Nothing -> "ID3v1"

instance Show ID3v1 where
    show t =
        ("Title: " ++ (form $ title t))                ++"\n"++
        ("Artist: " ++ (form $ artist t))              ++"\n"++
        ("Album: " ++ (form $ album t))                ++"\n"++
        ("Year: " ++ (form $ year t))                  ++"\n"++
        ("Comment: " ++ (form $ comment t))            ++
        (maybe "" (("\nTrack: " ++) . show) (track t)) ++
        (maybe "" ("\nGenre: " ++) (toGenre . genre $ t))
        where
            -- convert bytestring to initial printable ascii chars
            form = show . B.takeWhile (\c -> c >= 20 && c < 127)
