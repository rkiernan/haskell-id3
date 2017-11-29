{-# LANGUAGE OverloadedStrings #-}

module ID3v2
    ( ID3v2
    , id3v2
    ) where

import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString as AB
import Data.Bits

import Tag

data ID3v2 = ID3v2
    {
    tagHeader :: Header,
    tagExtHeader :: Maybe ExtHeader,
    frames :: [(FrameHeader, FrameBody)]
    }

data Header = Header
      {
      majorVersion :: Word8,
      minorVersion :: Word8,
      unsync :: Bool,
      extended :: Bool,
      experimental :: Bool,
      tagSize :: Int
      }

data FrameBody = UniqueFileIdentifier C.ByteString C.ByteString | TextInformation Word8 C.ByteString | URL C.ByteString

data ExtHeader = ExtHeader
    { extSize :: Int,
      crcDataPresent :: Bool,
      padding :: Int,
      crcData :: Maybe C.ByteString
    }

data FrameHeader = FrameHeader
    {
    frameID :: C.ByteString,
    frameSize :: Int,
    tagAlterPreservation :: Bool,
    fileAlterPreservation :: Bool,
    readOnly :: Bool,
    compression :: Bool,
    encryption :: Bool,
    groupingIdentity :: Bool
    }


-- assuming the indexing of testBit starts at the left (testbit [01] 1 -> true)
toNumber :: C.ByteString -> Int -> Int -> Int
toNumber bs remaining size =
  if remaining > 0 then
    if testBit bs (remaining-1)
      then
        2^(size-remaining) + toNumber bs (remaining-1) size
      else
        toNumber bs (remaining-1) size
  else 0

-- popCount from data.bits, gives set bits
notEmpty :: Word8 -> Bool
notEmpty w = (popCount w) > 0

-- for weird header sizing, size should be 28 each time
-- most significant bytes are not counted
sizeToNumber :: C.ByteString -> Int -> Int
sizeToNumber bs size = sizeToNumberAux bs size size 1 0

sizeToNumberAux bs size remaining bit power =
  if remaining > 0 then
    if bit == 8
      then
        sizeToNumberAux bs size (remaining-1) 1 power
      else
        if testBit bs (remaining-1)
          then
            2^(power) + sizeToNumberAux bs size (remaining-1) (bit+1) (power+1)
          else
            sizeToNumberAux bs size (remaining-1) (bit+1) (power+1)
  else 0

header :: Parser Header
header = do
  string "ID3"
  majorVersion <- anyWord8
  minorVersion <- anyWord8
  flags <- anyWord8
  size <- AB.take 4
  let unsync = testBit flags 0
  let extended = testBit flags 1
  let experimental = testBit flags 2
  let sizeResult = sizeToNumber size 28
  return $ Header majorVersion minorVersion unsync extended experimental sizeResult

id3v2 :: Parser ID3v2
id3v2 = do
    h <- header
    e <- if extended h then extHeader >>= Just else return Nothing
    return $ ID3v2 h e

extHeader :: Parser ExtHeader
extHeader = do
    size <- AB.take 4
    flags1 <- anyWord8
    flags2 <- anyWord8
    padding <- AB.take 4
    let hSize = toNumber size 32 32
    let crcFlag = testBit flags1 0
    let pSize = toNumber padding 32 32
    let crcData = if crcFlag then Just (AB.take 4) else Nothing
    return $ ExtHeader hSize crcFlag pSize crcData

frameHeader :: Parser FrameHeader
frameHeader = do
  frameID <- AB.take 4
  size <- AB.take 4
  flags1 <- anyWord8
  flags2 <- anyWord8
  let tagAlterPreservation = testBit flags1 0
  let fileAlterPreservation = testBit flags1 1
  let readOnly = testBit flags1 2
  let compression = testBit flags2 0
  let encryption = testBit flags2 1
  let groupingIdentity = testBit flags2 2
  let parsedSize = toNumber size 32 32
  return $ FrameHeader frameID parsedSize tagAlterPreservation fileAlterPreservation readOnly compression encryption groupingIdentity

frameBody :: String -> Int -> Parser FrameBody
frameBody "UFID" l = do
    ownerIdentifier <- AB.takeWhile notEmpty
    identifier <- AB.take (l - (C.length ownerIdentifier) -10) -- frame header has 10, so we subtract that
    return $ UniqueFileIdentifier ownerIdentifier identifier

-- since all text frames have the same format, it makes sense to have one parser
-- for TALB, TIT2, etc.
frameBody "TEXT" l = do
  encoding <- anyWord8
  info <- AB.take (l -11) -- 10 for header and one for encoding
  return $ TEXT encoding info

frameBody "URL" l = do
  url <- AB.take (l-10)
  return $ URL url
