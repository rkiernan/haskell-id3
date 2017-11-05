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
    , flags :: Bool
    , padding :: Int
    }

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
    return ExtHeader 0 false 0

