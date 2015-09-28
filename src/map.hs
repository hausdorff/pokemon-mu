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
import Row
import Transition

import Prelude hiding (foldl)
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
    show (Map grid _ _) = show grid

----------------------
-- Public functions --
----------------------

makeMap :: [String] -> Map
makeMap rows = Map { points      = grid
                 , outgoingEdges = makeOutgoingEdges grid
                 , incomingEdges = makeIncomingEdges grid
                 }
    where grid = Grid.makeGrid rows

-----------------------
-- Utility functions --
-----------------------

makeOutgoingEdges :: Grid -> AdjacencyMap
makeOutgoingEdges points = Grid.foldl processRow M.empty points
    where processRow acc row = Row.foldl processPoint acc row
          processPoint map point@(Point x y _) = M.insert point edges map
              where edges = Edgeset.fromList (neighborEdges points x y)

makeIncomingEdges :: Grid -> AdjacencyMap
makeIncomingEdges points = M.empty

----------------------
-- Helper functions --
----------------------

x (Point x _ _) = x
y (Point _ y _) = y
