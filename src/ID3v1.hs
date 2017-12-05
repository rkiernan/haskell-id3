{-# LANGUAGE OverloadedStrings #-}

module ID3v1
    ( ID3v1
    , emptyID3v1
    , id3v1
    , encodeV1
    ) where

import Prelude hiding (take)
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

emptyID3v1 :: ID3v1
emptyID3v1 = ID3v1 e e e y e Nothing 255
    where e = B.pack $ replicate 30 0
          y = B.pack $ replicate 4 0

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

addEmpty :: C.ByteString -> Int -> C.ByteString  
addEmpty bs i = if (C.length bs) == i then bs else addEmpty (C.append bs (B.singleton 0x00)) i


changeTitle :: ID3v1 -> String -> Maybe ID3v1
changeTitle t s = 
    if length s < 31 then let q = addEmpty (C.pack s) 30 in 
        Just $ ID3v1 
        (q)
        (artist t)
        (album t)
        (year t)
        (comment t)
        (track t)
        (genre t)
    else 
        Nothing 

changeArtist :: ID3v1 -> String -> Maybe ID3v1
changeArtist t s = 
    if length s < 31 then let q = addEmpty (C.pack s) 30 in 
        Just $ ID3v1 
        (title t)
        (q)
        (album t)
        (year t)
        (comment t)
        (track t)
        (genre t)
    else 
        Nothing 

changeAlbum :: ID3v1 -> String -> Maybe ID3v1
changeAlbum t s = 
    if length s < 31 then let q = addEmpty (C.pack s) 30 in 
        Just $ ID3v1 
        (title t)
        (artist t)
        (q)
        (year t)
        (comment t)
        (track t)
        (genre t)
    else 
        Nothing 

changeYear :: ID3v1 -> String -> Maybe ID3v1
changeYear t s = 
    if length s == 4 then let q = C.pack s in 
        Just $ ID3v1 
        (title t)
        (artist t)
        (album t)
        (q)
        (comment t)
        (track t)
        (genre t)
    else 
        Nothing 

changeComment :: ID3v1 -> String -> Maybe ID3v1
changeComment t s = 
    case (track t) of 
        Nothing ->
            if length s < 31 then let q = addEmpty (C.pack s) 30 in 
                Just $ ID3v1 
                (title t)
                (artist t)
                (album t)
                (year t)
                (q)
                (track t)
                (genre t)
            else 
                Nothing 
        _ ->
            if length s < 31 then let q = addEmpty (C.pack s) 28 in 
                Just $ ID3v1 
                (title t)
                (artist t)
                (album t)
                (year t)
                (q)
                (track t)
                (genre t)
            else 
                Nothing

changeTrack :: ID3v1 -> Int -> Maybe ID3v1
changeTrack t s = 
    if s < 256 then let q = fromInteger $ toInteger s in 
        Just $ ID3v1 
        (title t)
        (artist t)
        (album t)
        (year t)
        (comment t)
        (Just q)
        (genre t)
    else 
        Nothing 

changeGenre :: ID3v1 -> Int -> Maybe ID3v1
changeGenre t s = 
    if s < 256 then let q = fromInteger $ toInteger s in 
        Just $ ID3v1 
        (title t)
        (artist t)
        (album t)
        (year t)
        (comment t)
        (track t)
        (q)
    else 
        Nothing 
