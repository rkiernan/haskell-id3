module Main where

import qualified Data.ByteString.Char8 as C
import ID3v1

main :: IO ()
main = putStrLn . show $ emptyID3v1
