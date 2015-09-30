module Map (
      Map(..)
    , AdjacencyMap
    , makeMap
    , toTransition
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

data Map = Map { start         :: Point
               , end           :: Point
               , points        :: Grid
               , outgoingEdges :: AdjacencyMap
               , incomingEdges :: AdjacencyMap
               }

instance Show Map where
    show (Map _ _ grid _ _) = show grid

----------------------
-- Public functions --
----------------------

makeMap :: [String] -> Map
makeMap rows = Map { start         = makeStart grid
                   , end           = makeEnd grid
                   , points        = grid
                   , outgoingEdges = makeOutEdges grid
                   , incomingEdges = makeInEdges grid
                   }
    where grid = Grid.makeGrid rows

toTransition :: Map -> Transition
toTransition (Map _ _ points outEdges inEdges) = Up

-----------------------
-- Utility functions --
-----------------------

makeStart :: Grid -> Point
makeStart grid = case filteredPoints of
    (start:[]) -> start
    _          -> error "Invalid number of start positions in maze (should be 1)."
    where isStart point = (Point.getMapSquare point) == Entrance
          filteredPoints = filter isStart $ allPoints grid 

makeEnd :: Grid -> Point
makeEnd grid = case filteredPoints of
    (end:[]) -> end
    _          -> error "Invalid number of exit positions in maze (should be 1)."
    where isEnd point = (Point.getMapSquare point) == Exit
          filteredPoints = filter isEnd $ allPoints grid 


allPoints :: Grid -> [Point]
allPoints grid = concat $ map Row.toList $ Grid.toList grid

makeOutEdges :: Grid -> AdjacencyMap
makeOutEdges grid = makeEdges adjacentOutEdges grid

makeInEdges :: Grid -> AdjacencyMap
makeInEdges grid = makeEdges adjacentInEdges grid

makeEdges edgeGenerator points = Grid.foldl processRow AdjacencyMap.empty points
    where processRow accMap row                 = Row.foldl processPoint accMap row
          processPoint amap point@(Point x y _) = insert point edges amap
              where edges = Edgeset.fromList (edgeGenerator points x y)