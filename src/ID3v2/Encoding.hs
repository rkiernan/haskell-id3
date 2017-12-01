{-# LANGUAGE LambdaCase #-}

module ID3v2.Encoding
    ( Encoding (..)
    , getEncoding
    , getEncValue
    , parseEncoding
    , parseText
    ) where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Text
import Data.Text.Encoding
import Data.Word (Word8)

data Encoding =
      Latin     -- ISO-8859-1
    | Utf16Bom  -- UTF-16 with BOM (byte order mark)
    | Utf16     -- UTF-16BE
    | Utf8      -- UTF-8

getEncoding :: Word8 -> Encoding
getEncoding = \case
    0 -> Latin
    1 -> Utf16Bom
    2 -> Utf16       -- added for ID3v2.4
    3 -> Utf8        -- added for ID3v2.4
    _ -> Latin       -- default (not in spec)

getEncValue :: Encoding -> Word8
getEncValue = \case
    Latin    -> 0
    Utf16Bom -> 1
    Utf16    -> 2
    Utf8     -> 3

parseEncoding :: Parser Encoding
parseEncoding = getEncoding <$> satisfy (< 4)

parseText :: Encoding -> Parser Text
parseText = \case 
    Latin    -> decodeLatin1  <$> parseNull1
    Utf8     -> decodeUtf8    <$> parseNull1
    Utf16    -> decodeUtf16BE <$> parseNull2
    Utf16Bom -> parseBom

parseNull1 :: Parser B.ByteString
parseNull1 = takeTill (== 0)

parseNull2 :: Parser B.ByteString
parseNull2 = do
    b1 <- anyWord8
    b2 <- anyWord8
    case (b1,b2) of
        (0,0)   -> return B.empty
        (b1,b2) -> parseNull2 >>= 
                   (\e -> return $ B.append (B.pack [b1,b2]) e)

parseBom :: Parser Text
parseBom = do
    b1 <- satisfy (>= 0xfe)
    b2 <- satisfy (\b -> b >= 0xfe && b /= b1)
    case (b1,b2) of
        (0xff, 0xfe) -> decodeUtf16LE <$> parseNull2
        (0xfe, 0xff) -> decodeUtf16BE <$> parseNull2
        _ ->  decodeUtf16BE <$> parseNull2 -- should never happen

