module Edgeset (
      Edgeset
    , empty
    , keyFromEdge
    , keyFromPoints
    , fromList
    ) where

import Edge
import qualified Index as I
import Point
import Transition

import Prelude hiding (lookup)
import qualified Data.List as L hiding (lookup)
import qualified Data.Map as M

type Key = ((Int, Int), (Int, Int))
type Indexer = Edge -> Key
type Edgeset = I.IndexedSet Key Edge

----------------------
-- Public functions --
----------------------

empty :: I.IndexedSet Key Edge
empty = I.empty keyFromEdge unify

fromList :: [Edge] -> I.IndexedSet Key Edge
fromList edges = I.fromFoldable keyFromEdge unify edges

-----------------------
-- Private functions --
-----------------------

keyFromEdge (Edge (Point x1 y1 _) (Point x2 y2 _) _) = ((x1, y1), (x2, y2))

keyFromPoints (Point x1 y1 _) (Point x2 y2 _) = ((x1, y1), (x2, y2))
