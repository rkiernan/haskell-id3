module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import ID3v1

main :: IO ()
main = do
    tag <- B.readFile "res/id3v1-0.tag"
    case parseOnly id3v1 tag of
        Left  s -> putStrLn s
        Right r -> putStrLn (show r)
