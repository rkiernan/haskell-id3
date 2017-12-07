{-# LANGUAGE OverloadedStrings #-}

module ID3v1
    ( ID3v1
    , id3v1
    , encodeV1
    ) where

import Prelude hiding (take)
import qualified Prelude as P (take)
import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.ByteString.Builder as BD
import qualified Data.ByteString.Lazy as L

import ID3v1.Genre
import Tag

data ID3v1 = ID3v1
    { title   :: C.ByteString
    , artist  :: C.ByteString
    , album   :: C.ByteString
    , year    :: C.ByteString
    , comment :: C.ByteString
    , track   :: Maybe Word8 -- ID3v1.1 adds track field
    , genre   :: Word8
    } deriving Eq
--TODO add support for ID3v1.2 which adds extended tag

encodeV1 :: ID3v1 -> L.ByteString
encodeV1 = toLazyByteString . renderv1

renderv1 :: ID3v1 -> Builder 
renderv1 tag = case (track tag) of 
    Nothing -> 
        mconcat ((string8 "TAG"):
        (BD.byteString (title tag)):
        (BD.byteString (artist tag)):
        (BD.byteString (album tag)):
        (BD.byteString (year tag)):
        (BD.byteString (comment tag)):
        (BD.word8 (genre tag)):[])
    Just t ->
        mconcat ((string8 "TAG"):
        (BD.byteString (title tag)):
        (BD.byteString (artist tag)):
        (BD.byteString (album tag)):
        (BD.byteString (year tag)):
        (BD.byteString (comment tag)):
        (BD.word8 (0x00)):
        (BD.word8 (t)):
        (BD.word8 (genre tag)):[])


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

addEmpty :: Int -> C.ByteString -> C.ByteString  
addEmpty i bs =
    let diff = i - (C.length bs) in 
    C.append bs (B.replicate diff 0)

instance Tag ID3v1 where
    version t =
        case (track t) of
            Just _ -> "ID3v1.1"
            Nothing -> "ID3v1"

    emptyTag = ID3v1 e e e y e Nothing 255
      where e = B.pack $ replicate 30 0
            y = B.pack $ replicate 4 0

    setTitle  t x = x {title = sform 30 t}
    setArtist a x = x {artist = sform 30 a}
    setAlbum  a x = x {album = sform 30 a}
    setYear   y x = x {year = sform 4 y}
    setComment c x = 
        case (track x) of Just _ -> x {comment = sform 28 c}
                          Nothing -> x {comment = sform 30 c}
    setGenre g x = x {genre = fromGenre g}
    setTrack t x = x {comment = B.take 28 (comment x),
                      track = Just . fromInteger . toInteger $ t}
    
    getTitle   = Just . title
    getArtist  = Just . artist
    getAlbum   = Just . album
    getYear    = Just . year
    getComment = Just . comment
    getGenre   = toGenre . genre
    getTrack x = do
        t <- track x
        return $ fromInteger $ toInteger t

sform n = addEmpty n . C.pack . P.take n
