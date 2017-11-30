{-# LANGUAGE OverloadedStrings #-}

module ID3v2
    ( ID3v2
    , id3v2
    , Header
    , header
    , FrameHeader
    , frameHeader
    ) where

import Prelude hiding (take, takeWhile)
import Data.Attoparsec.ByteString
import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Bits

import Tag
import ID3v2.Sync

data ID3v2 = ID3v2
    {
    tagHeader :: Header,
    tagExtHeader :: Maybe ExtHeader,
    frames :: [Frame]
    }

data Header = Header
    {
    majorVersion :: Word8,
    minorVersion :: Word8,
    unsync :: Bool,
    extended :: Bool,
    experimental :: Bool,
    tagSize :: Integer
    }

data Frame = Frame
    {
    fHeader :: FrameHeader,
    fBody :: FrameBody
    }

data FrameBody = UniqueFileIdentifier C.ByteString C.ByteString | TextInformation Word8 C.ByteString | URL C.ByteString

data ExtHeader = ExtHeader
    { extSize :: Integer,
      crcDataPresent :: Bool,
      padding :: Integer,
      crcData :: Maybe C.ByteString
    }

data FrameHeader = FrameHeader
    {
    frameID :: C.ByteString,
    frameSize :: Integer,
    tagAlterPreservation :: Bool,
    fileAlterPreservation :: Bool,
    readOnly :: Bool,
    compression :: Bool,
    encryption :: Bool,
    groupingIdentity :: Bool
    }

-- popCount from data.bits, gives set bits
notEmpty :: Word8 -> Bool
notEmpty w = (popCount w) > 0

-- Gives type given id
frameType :: C.ByteString -> String
frameType s = frameType' $ C.unpack s
  where
    frameType' "UFID" = "UFID"
    frameType' "TXXX" = "USER"
    frameType' ('T':_) = "TEXT"
    frameType' ('W':_) = "URL"

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
    return $ Header majorv minorv unsync extend exper size

extHeader :: Parser ExtHeader
extHeader = do
    hsize <- wordsToInteger <$> count 4 anyWord8
    flags1 <- anyWord8
    flags2 <- anyWord8
    psize <- wordsToInteger <$> count 4 anyWord8
    let crcFlag = testBit flags1 0
    crcData <- if crcFlag then (take 4) >>= return . Just else return Nothing
    return $ ExtHeader hsize crcFlag psize crcData

frame :: Parser Frame
frame = do
    h <- frameHeader
    b <- frameBody (frameType $ frameID h) (fromIntegral $ frameSize h)
    return $ Frame h b

frameHeader :: Parser FrameHeader
frameHeader = do
    frameID <- take 4
    size <- wordsToInteger <$> count 4 anyWord8
    flags1 <- anyWord8
    flags2 <- anyWord8
    let tagAlterPreservation = testBit flags1 0
    let fileAlterPreservation = testBit flags1 1
    let readOnly = testBit flags1 2
    let compression = testBit flags2 0
    let encryption = testBit flags2 1
    let groupingIdentity = testBit flags2 2
    return $ FrameHeader frameID size tagAlterPreservation fileAlterPreservation readOnly compression encryption groupingIdentity

frameBody :: String -> Int -> Parser FrameBody
frameBody "UFID" l = do
    ownerIdentifier <- takeWhile notEmpty
    identifier <- take (l - (C.length ownerIdentifier))
    return $ UniqueFileIdentifier ownerIdentifier identifier

-- since all text frames have the same format, it makes sense to have one parser
-- for TALB, TIT2, etc.
frameBody "TEXT" l = do
    encoding <- anyWord8
    info <- take (l -1) -- 10 for header and one for encoding
    return $ TextInformation encoding info

frameBody "URL" l = do
    url <- take (l)
    return $ URL url


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
          ("Frame ID: " ++ (form $ frameID t)) ++"\n"++
          ("Frame Size: " ++ (show $ frameSize t)) ++"\n"++
          ("Tag Alter Preservation: " ++ (show $ tagAlterPreservation t)) ++"\n"++
          ("File Alter Preservation: " ++ (show $ fileAlterPreservation t)) ++"\n"++
          ("Read Only: " ++ (show $ readOnly t)) ++"\n"++
          ("Compressed: " ++ (show $ compression t)) ++"\n"++
          ("Encryption: " ++ (show $ encryption t)) ++"\n"++
          ("Grouping Identity: " ++ (show $ groupingIdentity t))

instance Show FrameBody where
    show t = case t of
        TextInformation e text ->
            ("Encoding: " ++ (show $ e)) ++"\n"++
            ("Text Information: " ++ (form $ text))
