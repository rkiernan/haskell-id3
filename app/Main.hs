module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import ID3v1
import ID3v2

main :: IO ()
main = do
    runParser id3v1 "res/id3v1-0.tag"
    runParser frameHeader "res/freeBirdFrameHeader1.tag"

runParser :: (Show a) => Parser a -> FilePath -> IO ()
runParser p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> putStrLn (show r)

