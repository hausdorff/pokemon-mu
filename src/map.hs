module Map (
      Map(..)
    , AdjacencyMap
    , makeMap
    ) where

import AdjacencyMap
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
makeMap rows = Map { points        = grid
                   , outgoingEdges = makeOutEdges grid
                   , incomingEdges = makeInEdges grid
                   }
    where grid = Grid.makeGrid rows

-----------------------
-- Utility functions --
-----------------------

makeOutEdges :: Grid -> AdjacencyMap
makeOutEdges grid = makeEdges adjacentOutEdges grid

makeInEdges :: Grid -> AdjacencyMap
makeInEdges grid = makeEdges adjacentInEdges grid

makeEdges edgeGenerator points = Grid.foldl processRow AdjacencyMap.empty points
    where processRow accMap row                 = Row.foldl processPoint accMap row
          processPoint amap point@(Point x y _) = insert point edges amap
              where edges = Edgeset.fromList (edgeGenerator points x y)