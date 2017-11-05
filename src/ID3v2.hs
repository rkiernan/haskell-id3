{-# LANGUAGE OverloadedStrings #-}

module ID3v2
    ( ID3v2
    , id3v2
    ) where

import Data.Word (Word8)
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Bits

import Tag

data ID3v2 = ID3v2
    { tagHeader :: Header
    , tagExtHeader :: Maybe ExtHeader
    }

data Header = Header
      {
      majorVersion :: Word8,
      minorVersion :: Word8,
      unsync :: Bool,
      extended :: Bool,
      experimental :: Bool,
      size :: Int
      }

data ExtHeader = ExtHeader
    { extSize :: Int
    , crcData :: Bool
    , padding :: Int,
      crcData :: Maybe C.ByteString
    }

data FrameHeader = FrameHeader
    {
    frameID :: C.ByteString,
    size :: Int,
    tagAlterPreservation :: Bool,
    fileAlterPreservation :: Bool,
    readOnly :: Bool,
    compression :: Bool,
    encryption :: Boool,
    groupingIdentity :: Bool
    }

toNumber :: C.ByteString -> Int -> Int -> Int
toNumber bs remaining size =
  if remaining > 0
    if testBit bs remaining-1
      then
        2^(size-remaining) + toNumber bs (remaining-1) size
      else
        toNumber bs (remaining-1) size
  else 0

-- for weird header sizing
--sizeToNumber bs remaining multiplierOffset size =
--  if remaining > 0 then
--    if remaining
--    let multiplier = size-remaining-multiplierOffset in
--    if
--  else 0

header :: Parser Header
header = do
  string "ID3"
  majorVersion <- anyWord8
  minorVersion <- anyWord8
  flags <- anyWord8
  size <- take 4
  let unsync = testBit flags 0
  let extended = testBit flags 1
  let experimental = testBit flags 2

  return $ Header majorVersion minorVersion unsync extended experimental sizeResult

id3v2 :: Parser ID3v2
id3v2 = do
    h <- header
    e <- if extended h then extHeader >>= Just else return Nothing
    return $ ID3v2 h e

extHeader :: Parser ExtHeader
extHeader = do
    size <- take 4
    flags1 <- anyWord8
    flags2 <- anyWord8
    padding <- take 4
    let hSize = toNumber size 32 32
    let crcFlag = testBit flags1 0
    let pSize = toNumber padding 32 32
    let crcData = if crcFlag then take 4 else Nothing
    return ExtHeader hSize crcFlag pSize crcData

frameHeader :: Parser FrameHeader
frame = do
  frameID <- take 4
  size <- take 4
  flags1 <- anyWord8
  flags2 <- anyWord8
  let tagAlterPreservation = testBit flags1 0
  let fileAlterPreservation = testBit flags1 1
  let readOnly = testBit flags1 2
  let compression = testBit flags2 0
  let encryption = testBit flags2 1
  let groupingIdentity = testBit flags2 2
  let parsedSize = toNumber size 32 32
  return FrameHeader frameID parsedSize tagAlterPreservation fileAlterPreservation readOnly compression encryption groupingIdentity
