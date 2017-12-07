module Main where 

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8 as C


import Data.Text as T
import ID3v1
import ID3v2
import Test.Hspec
import ID3v2.Encoding

main :: IO ()
main = hspec $ do
	describe "Verify ID3v1 parse is accurate" $ do 
		it "Correct v1 title" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getTitle r ) `shouldBe` (addEmpty (C.pack "Yellow Submarine") 30)) 

		it "Correct v1 artist" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getArtist r ) `shouldBe` (addEmpty (C.pack "Beatles") 30)) 

		it "Correct v1 album" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getAlbum r ) `shouldBe` (addEmpty (C.pack "Revolver") 30)) 

		it "Correct v1 year" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v1.getYear r ) `shouldBe`  (C.pack "1966") 

	describe "Verify ID3v2 parse is accurate" $ do 
		it "Correct v2 title 1" $ do 
			bs <- B.readFile "res/freeBird.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getTitle r ) `shouldBe` (Just $ T.pack "Free Bird") 

		it "Correct v2 title 2" $ do 
			bs <- B.readFile "res/billieJean.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getTitle r ) `shouldBe` (Just $ T.pack "Billie Jean (Single Version)")

		it "Correct v2 artist 1" $ do 
			bs <- B.readFile "res/freeBird.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getArtist r ) `shouldBe` (Just $ T.pack "Lynyrd Skynyrd") 

		it "Correct v2 artist 2" $ do 
			bs <- B.readFile "res/billieJean.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getArtist r ) `shouldBe` (Just $ T.pack "Michael Jackson") 

		it "Correct v2 album 1" $ do 
			bs <- B.readFile "res/freeBird.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getAlbum r ) `shouldBe` (Just $ T.pack "The Essential Lynyrd Skynyrd")

		it "Correct v2 album 2" $ do 
			bs <- B.readFile "res/billieJean.mp3"
			case parseOnly id3v2 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v2.getAlbum r ) `shouldBe` (Just $ T.pack "Thriller (Google Play Exclusive Version)")

	describe "Verify ID3v1 editor" $ do 
		it "Correct artist edit" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> let t = ID3v1.changeArtist r "Ryan" in case t of 
					Nothing -> 1 `shouldBe` 0
					Just p  -> ((ID3v1.getArtist p ) `shouldBe` (addEmpty (C.pack "Ryan") 30))

		it "Correct album edit" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> let t = ID3v1.changeAlbum r "Ryan's Album" in case t of 
					Nothing -> 1 `shouldBe` 0
					Just p  -> ((ID3v1.getAlbum p ) `shouldBe` (addEmpty (C.pack "Ryan's Album") 30)) 

		it "Correct year edit" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> let t = ID3v1.changeYear r "2000" in case t of 
					Nothing -> 1 `shouldBe` 0
					Just p  -> (ID3v1.getYear p ) `shouldBe`  (C.pack "2000") 

	describe "Verify ID3v1 writer" $ do 
		it "Correct title write" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> B.writeFile "res/test.tag" (Data.ByteString.Lazy.toStrict $ encodeV1 r)
			bs' <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs' of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getTitle r ) `shouldBe` (addEmpty (C.pack "Yellow Submarine") 30))  

		it "Correct artist write" $ do 
			bs <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getArtist r ) `shouldBe` (addEmpty (C.pack "Beatles") 30)) 

		it "Correct album write" $ do 
			bs <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getAlbum r ) `shouldBe` (addEmpty (C.pack "Revolver") 30)) 

		it "Correct year write" $ do 
			bs <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v1.getYear r ) `shouldBe`  (C.pack "1966") 

	describe "Verify ID3v1 write after edit" $ do 
		it "Working write after edit" $ do 
			bs <- B.readFile "res/id3v1-0.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> let t = ID3v1.changeArtist r "Ryan" in case t of 
					Nothing -> 1 `shouldBe` 0
					Just p  -> let t = ID3v1.changeYear p "2000" in case t of 
						Nothing -> 1 `shouldBe` 0
						Just q  -> B.writeFile "res/test.tag" (Data.ByteString.Lazy.toStrict $ encodeV1 q)

		it "Correct artist write after edit" $ do 
			bs <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> ((ID3v1.getArtist r ) `shouldBe` (addEmpty (C.pack "Ryan") 30)) 

		it "Correct year write after edit" $ do 
			bs <- B.readFile "res/test.tag"
			case parseOnly id3v1 bs of 
				Left  s -> 1 `shouldBe` 0
				Right r -> (ID3v1.getYear r ) `shouldBe` (C.pack "2000") 
 

addEmpty :: C.ByteString -> Int -> C.ByteString  
addEmpty bs i = if (C.length bs) == i then bs else addEmpty (C.append bs (B.singleton 0x00)) i

runParser' :: Parser a -> FilePath -> IO (Maybe a)
runParser' p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> return $ Nothing
        Right r -> return $ Just r

runParser :: (Show a) => Parser a -> FilePath -> IO ()
runParser p f = do
    bs <- B.readFile f
    case parseOnly p bs of
        Left  s -> putStrLn s
        Right r -> putStrLn (show r)

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