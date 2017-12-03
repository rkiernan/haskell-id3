{-# LANGUAGE OverloadedStrings #-}

module ID3v2
    ( ID3v2
    , id3v2
    , Header
    , header
    , FrameHeader
    , frameHeader
    , encodeV2
    ) where

import Prelude hiding (take, takeWhile)
import Data.Attoparsec.ByteString
import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.ByteString.Builder as BD
import Data.Bits
import Data.Text.Encoding

import Tag
import ID3v2.Sync
import ID3v2.Encoding

data ID3v2 = ID3v2
    { tagHeader :: Header
    , tagExtHeader :: Maybe ExtHeader
    , frames :: [Frame]
    }

data Header = Header
    { majorVersion :: Word8
    , minorVersion :: Word8
    , headerFlags :: Word8
    , unsync :: Bool
    , extended :: Bool
    , experimental :: Bool
    , tagSize :: Integer
    }

data ExtHeader = ExtHeader
    { extSize :: Integer
    , extHeaderFlags1 :: Word8
    , extHeaderFlags2 :: Word8
    , crcDataPresent :: Bool
    , padding :: Integer
    , crcData :: Maybe C.ByteString
    }

data Frame = Frame
    { fHeader :: FrameHeader
    , fBody :: FrameBody
    }

data FrameHeader = FrameHeader
    { frameID :: String
    , frameSize :: Integer
    , frameFlags1 :: Word8
    , tagAlterPreservation :: Bool
    , fileAlterPreservation :: Bool
    , readOnly :: Bool
    , frameFlags2 :: Word8
    , compression :: Bool
    , encryption :: Bool
    , groupingIdentity :: Bool
    }

data FrameBody = 
      UFID T.Text [Word8]
    | Text Encoding T.Text
    | URL T.Text
    | PRIV T.Text [Word8]

-- popCount from data.bits, gives set bits
notEmpty :: Word8 -> Bool
notEmpty w = (popCount w) > 0

id3v2 :: Parser ID3v2
id3v2 = do
    h <- header
    e <- if extended h then extHeader >>= return . Just else return Nothing
    f <- many1 frame
    return $ ID3v2 h e f

header :: Parser Header
header = do
    string "ID3"
    majorv <- anyWord8
    minorv <- anyWord8
    flags <- anyWord8
    let unsync = testBit flags 0
    let extend = testBit flags 1
    let exper  = testBit flags 2
    size <- unsynchronise <$> count 4 anyWord8
    return $ Header majorv minorv flags unsync extend exper size

extHeader :: Parser ExtHeader
extHeader = do
    hsize <- wordsToInteger <$> count 4 anyWord8
    flags1 <- anyWord8
    flags2 <- anyWord8
    psize <- wordsToInteger <$> count 4 anyWord8
    let crcFlag = testBit flags1 0
    crcData <- if crcFlag then (take 4) >>= return . Just else return Nothing
    return $ ExtHeader hsize flags1 flags2 crcFlag psize crcData

frame :: Parser Frame
frame = do
    h <- frameHeader
    b <- frameBody (frameID h) (fromIntegral $ frameSize h)
    return $ Frame h b

frameHeader :: Parser FrameHeader
frameHeader = do
    frameID <- C.unpack <$> take 4
    size <- wordsToInteger <$> count 4 anyWord8
    flags1 <- anyWord8
    flags2 <- anyWord8
    let tagAlterPreservation = testBit flags1 0
    let fileAlterPreservation = testBit flags1 1
    let readOnly = testBit flags1 2
    let compression = testBit flags2 0
    let encryption = testBit flags2 1
    let groupingIdentity = testBit flags2 2
    return $ FrameHeader frameID size flags1 tagAlterPreservation fileAlterPreservation readOnly flags2 compression encryption groupingIdentity

frameBody :: String -> Int -> Parser FrameBody
frameBody "UFID" l = do
    id <- parseText Latin Nothing
    identifier <- many' anyWord8
    return $ UFID id identifier

-- for TALB, TIT2, etc.
frameBody ('T':_) l = do
    enco <- parseEncoding
    info <- parseText enco (Just l)
    return $ Text enco info

frameBody "URL" l = do
    url <- parseText Latin Nothing --URLs always use ISO-8859-1
    return $ URL url

frameBody "PRIV" l = do
    id <- parseText Latin Nothing
    privateData <- many' anyWord8
    return $ PRIV id privateData

frameBody _ l = do
    enco <- parseEncoding
    info <- parseText enco (Just l)
    return $ Text enco info

encodeV2 :: ID3v2 -> L.ByteString
encodeV2 = toLazyByteString . renderV2

renderV2 :: ID3v2 -> Builder
renderV2 tag =
    case (tagExtHeader tag) of
        Nothing ->
            mconcat ((string8 "ID3"):
            (BD.word8 $ majorVersion $ tagHeader tag):
            (BD.word8 $ minorVersion $ tagHeader tag):
            (BD.word8 $ headerFlags  $ tagHeader tag):
            (BD.byteString $ B.pack $ synchronise 4 $ tagSize $ tagHeader tag):
            (renderFrames (frames tag)))
        _ ->
            mconcat ((string8 "ID3"):
            (BD.word8 $ majorVersion $ tagHeader tag):
            (BD.word8 $ minorVersion $ tagHeader tag):
            (BD.word8 $ headerFlags  $ tagHeader tag):
            (BD.byteString $ B.pack $ synchronise 4 $ tagSize $ tagHeader tag):[])

renderFrames :: [Frame] -> [Builder]
renderFrames []     = []
renderFrames (x:xs) = (renderFrame x):(renderFrames xs)

renderFrame :: Frame -> Builder
renderFrame f = mconcat ((renderFrameHeader (fHeader f)):(renderFrameBody (fBody f)):[])

renderFrameHeader :: FrameHeader -> Builder
renderFrameHeader fh =
    mconcat $
    ((string8 $ frameID fh):
    (BD.byteString $ B.pack $ integerToWords 4 (frameSize fh)):
    (BD.word8 $ frameFlags1 fh):
    (BD.word8 $ frameFlags2 fh):[])

renderFrameBody :: FrameBody -> Builder
renderFrameBody fb = case fb of
    UFID t b ->
        mconcat $
        (BD.byteString $ C.pack $ T.unpack t):
        (BD.byteString $ C.singleton '0'):
        ((BD.byteString $ B.pack b):[])
    Text e t -> case e of
        Latin ->
            mconcat $
            (BD.byteString $ C.singleton '0'):
            ((BD.byteString $ C.pack $ T.unpack t):[])
        Utf16Bom ->
            mconcat $
            (BD.byteString $ C.singleton '1'):
            ((BD.byteString $ encodeUtf16BE t):[])
    URL t    ->
        (BD.byteString $ C.pack $ T.unpack t)
    PRIV t b ->
        mconcat $
        (BD.byteString $ C.pack $ T.unpack t):
        (BD.byteString $ C.singleton '0'):
        ((BD.byteString $ B.pack b):[])


form = show . B.takeWhile (\c -> c >= 20 && c < 127)

instance Show Header where
    show t =
        ("Major Version: " ++ (show $ majorVersion t)) ++"\n"++
        ("Minor Version: " ++ (show $ minorVersion t)) ++"\n"++
        ("Unsync: " ++ (show $ unsync t))              ++"\n"++
        ("Experimental: " ++ (show $ experimental t))  ++"\n"++
        ("Tag Size: " ++ (show $ tagSize t))

instance Show FrameHeader where
    show t =
          ("Frame ID: " ++ (frameID t)) ++"\n"++
          ("Frame Size: " ++ (show $ frameSize t)) ++"\n"++
          ("Tag Alter Preservation: " ++ (show $ tagAlterPreservation t)) ++"\n"++
          ("File Alter Preservation: " ++ (show $ fileAlterPreservation t)) ++"\n"++
          ("Read Only: " ++ (show $ readOnly t)) ++"\n"++
          ("Compressed: " ++ (show $ compression t)) ++"\n"++
          ("Encryption: " ++ (show $ encryption t)) ++"\n"++
          ("Grouping Identity: " ++ (show $ groupingIdentity t))

instance Show Frame where
    show t =
        (show $ fHeader t) ++"\n"++
        (show $ fBody t)

instance Show FrameBody where
    show t = case t of
        Text e text ->
            ("Text Information: " ++ (show text))
        PRIV o d ->
            ("Owner Identifier: " ++ (show $ o))

instance Show ID3v2 where
    show t =
        (show $ tagHeader t )  ++"\n"++
        (showEach $ frames t )
        where
            showEach []   = ""
            showEach (x:xs) = (show x) ++"\n\n"++ (showEach xs)
