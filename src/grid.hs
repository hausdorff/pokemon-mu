module Grid (
      Grid
    , Row
    , getPoint
    , getOutEdge
    , getInEdge
    , Grid.foldl
    , makeGrid
    , Grid.toList
    , adjacentOutEdges
    , adjacentInEdges
    ) where

import Edge
import Edgeset
import MapSquare
import Point
import Row
import Transition

import Prelude hiding (Left, Right, foldl)
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V


newtype Grid = Grid (V.Vector Row)

instance Show Grid where
    show (Grid rows) = unlines $ V.toList (V.map convertRow rows)
        where convertRow = map Point.toChar . Row.toList

----------------------
-- Public functions --
----------------------

getPoint :: Grid -> Int -> Int -> Maybe Point
getPoint (Grid rows) x y = do
    row <- rows V.!? y
    row !? x

getOutEdge :: Grid -> Int -> Int -> Transition -> Maybe Edge
getOutEdge grid x y direction = do
    from <- getPoint grid x y
    to <- getPoint grid x' y'
    makeEdge from to direction
    where (x', y') = case direction of
              Up    -> (x, y - 1)
              Down  -> (x, y + 1)
              Left  -> (x - 1, y)
              Right -> (x + 1, y)

getInEdge :: Grid -> Int -> Int -> Transition -> Maybe Edge
getInEdge grid x y direction = do
    to <- getPoint grid x y
    from <- getPoint grid x' y'
    makeEdge from to direction
    where (x', y') = case direction of
              -- NOTE: these are reversed from `getOutEdge`.
              Up    -> (x, y + 1)
              Down  -> (x, y - 1)
              Left  -> (x + 1, y)
              Right -> (x - 1, y)

foldl :: (a -> Row -> a) -> a -> Grid -> a
foldl f seed (Grid rows) = V.foldl f seed rows

makeGrid :: [String] -> Grid
makeGrid rows = as2dVect
    where makePoint x y c = Point x y (fromChar c)
          toRow (y, row)  = Row.fromList $ map (\(x, c) -> makePoint x y c) (indexed row)
          as2dVect        = Grid $ V.fromList $ map toRow (revindexed rows)

toList :: Grid -> [Row]
toList (Grid rows) = V.toList rows

adjacentOutEdges :: Grid -> Int -> Int -> [Edge]
adjacentOutEdges grid x y = adjacentEdges getOutEdge grid x y

adjacentInEdges :: Grid -> Int -> Int -> [Edge]
adjacentInEdges grid x y = adjacentEdges getInEdge grid x y

-----------------------
-- Private functions --
-----------------------

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

revindexed :: [a] -> [(Int, a)]
revindexed xs = zip indexes xs
    where indexes = reverse $ take (length xs) [0..]

adjacentEdges edgeGenerator grid x y = Maybe.catMaybes neighbors
    where directions = [Up, Down, Left, Right]
          neighbors  = map (edgeGenerator grid x y) directions
