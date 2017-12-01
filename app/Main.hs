module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import ID3v1
import ID3v2

main :: IO ()
main = do
    parseTest id3v1 "res/id3v1-0.tag"
    parseTest frameHeader "res/freeBirdFrameHeader1.tag"
