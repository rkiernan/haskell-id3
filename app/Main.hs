module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy


import ID3v1
import ID3v2

main :: IO ()
main = do
    runParser id3v2 "res/test.tag"

runParser :: (Show a) => Parser a -> FilePath -> IO ()
runParser p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> putStrLn (show r)

runEncoder :: Parser ID3v2 -> FilePath -> IO ()
runEncoder p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> B.writeFile "res/test.tag" (Data.ByteString.Lazy.toStrict $ encodeV2 r)
 