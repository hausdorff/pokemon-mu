module Row (
      Row
    , foldl
    , (!?)
    , toList
    , fromList
    ) where

import Point

import Prelude hiding (foldl)
import qualified Data.Vector as V

newtype Row = Row (V.Vector Point)

foldl :: (a -> Point -> a) -> a -> Row -> a
foldl f seed (Row row) = V.foldl f seed row

(!?) :: Row -> Int -> Maybe Point
(!?) (Row row) i = row V.!? i

toList :: Row -> [Point]
toList (Row row) = V.toList row

fromList :: [Point] -> Row
fromList row = Row (V.fromList row)