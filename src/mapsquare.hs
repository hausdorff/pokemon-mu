module MapSquare (
      MapSquare(..)
    , fromChar
    , toChar
    ) where

data MapSquare = Entrance | Exit | Wall | Grass | Ground
    deriving (Eq, Ord)

instance Show MapSquare where
    show Entrance = "S"
    show Exit     = "E"
    show Wall     = "#"
    show Ground   = " "

instance Read MapSquare where
    readsPrec _ "S" = [(Entrance, "")]
    readsPrec _ "E" = [(Exit, "")]
    readsPrec _ "#" = [(Wall, "")]
    readsPrec _ " " = [(Ground, "")]

fromChar :: Char -> MapSquare
fromChar c = read [c]

toChar :: MapSquare -> Char
toChar s = case (show s) of
    [c] -> c
