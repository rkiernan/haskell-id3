module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy


import ID3v1
import ID3v2


-- test1
--	runParser id3v1 "res/id3v1-0.tag"
--	getLine
--	runParser id3v2 "res/freeBird.mp3"
--	getLine
--	runEncoder id3v2 "res/freeBird.mp3"
--	getLine
--	runParser id3v2 "res/test.tag"

main :: IO ()
main = do
    getLine
    runParser id3v1 "res/id3v1-0.tag"

    getLine
    runParser id3v2 "res/freeBird.mp3"

    getLine
    runEncoder2 id3v2 "res/freeBird.mp3"

    getLine
    runParser id3v2 "res/test.tag"

runParser :: (Show a) => Parser a -> FilePath -> IO ()
runParser p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> putStrLn (show r)

runParser' :: Parser a -> FilePath -> IO (Maybe a)
runParser' p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> return $ Nothing
        Right r -> return $ Just r

runEncoder2 :: Parser ID3v2 -> FilePath -> IO ()
runEncoder2 p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> B.writeFile "res/test.tag" (Data.ByteString.Lazy.toStrict $ encodeV2 r)

runEncoder1 :: Parser ID3v1 -> FilePath -> IO ()
runEncoder1 p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> B.writeFile "res/test.tag" (Data.ByteString.Lazy.toStrict $ encodeV1 r)
