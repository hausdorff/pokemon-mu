module Map (
      Map(..)
    , AdjacencyMap(..)
    , makeMap
    ) where

import Edge
import Edgeset
import Grid
import MapSquare
import Point
import Transition

import qualified Data.Map as M
import qualified Data.Vector as V

--------------------
-- Map data types --
--------------------

type AdjacencyMap = M.Map Point Edgeset

data Map = Map { points        :: Grid
               , outgoingEdges :: AdjacencyMap
               , incomingEdges :: AdjacencyMap
               }

instance Show Map where
    show (Map rows _ _) = unlines $ V.toList (V.map convertRow rows)
        where convertRow = map Point.toChar . V.toList

----------------------
-- Public functions --
----------------------

makeMap rows = Map { points      = as2dVect
                 , outgoingEdges = makeOutgoingEdges as2dVect
                 , incomingEdges = makeIncomingEdges as2dVect
                 }
    where mkPt x y c           = Point x y (fromChar c)
          rowToPoints (y, row) = V.fromList $ map (\(x, c) -> mkPt x y c) (indexed row)
          as2dVect             = V.fromList $ map rowToPoints (revindexed rows)

-----------------------
-- Utility functions --
-----------------------

makeOutgoingEdges :: Grid -> AdjacencyMap
makeOutgoingEdges points = V.foldl processRow M.empty points
    where processRow acc row  = V.foldl processPoint acc row
          processPoint map point@(Point x y _) = M.insert point edges map
              where edges = fromList (neighborEdges points x y)

makeIncomingEdges :: Grid -> AdjacencyMap
makeIncomingEdges points = M.empty

----------------------
-- Helper functions --
----------------------

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

revindexed :: [a] -> [(Int, a)]
revindexed xs = zip indexes xs
    where indexes = reverse $ take (length xs) [0..]

x (Point x _ _) = x
y (Point _ y _) = y
