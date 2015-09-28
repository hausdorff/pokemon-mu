module Grid (
      Grid(..)
    , getPoint
    , getEdge
    , neighborEdges
    ) where

import Edge
import Point
import Transition

import Prelude hiding (Left, Right)
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Point)

getPoint :: Grid -> Int -> Int -> Maybe Point
getPoint points x y = do
    row <- points V.!? y
    row V.!? x

getEdge :: Grid -> Int -> Int -> Transition -> Maybe Edge
getEdge points x y direction = do
    from <- getPoint points x y
    to <- getPoint points x' y'
    makeEdge from to direction
    where (x', y') = case direction of
              Up    -> (x, y - 1)
              Down  -> (x, y + 1)
              Left  -> (x - 1, y)
              Right -> (x + 1, y)

neighborEdges :: Grid -> Int -> Int -> [Edge]
neighborEdges points x y = Maybe.catMaybes neighbors
    where directions = [Up, Down, Left, Right]
          neighbors  = map (getEdge points x y) directions
