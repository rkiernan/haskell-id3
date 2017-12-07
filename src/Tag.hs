module Tag
    ( Tag (..)
    ) where

import Data.ByteString

class Tag a where
    version :: a -> String
    emptyTag :: a
    
    -- modifiers
    setTitle   :: String -> a -> a
    setArtist  :: String -> a -> a
    setAlbum   :: String -> a -> a
    setYear    :: String -> a -> a
    setComment :: String -> a -> a
    setGenre   :: String -> a -> a
    setTrack   :: Int -> a -> a
    
    -- accessors
    getTitle   :: a -> Maybe ByteString
    getArtist  :: a -> Maybe ByteString
    getAlbum   :: a -> Maybe ByteString
    getYear    :: a -> Maybe ByteString
    getComment :: a -> Maybe ByteString
    getGenre   :: a -> Maybe String
    getTrack   :: a -> Maybe Int

