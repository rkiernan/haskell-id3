module Tag
    ( Tag,
      version

    ) where

class Tag a where
	version :: a -> String

