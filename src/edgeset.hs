module Edgeset (
      Edgeset
    , empty
    , unify
    , unifyAll
    , lookup
    , insert
    , Edgeset.foldl
    , fromList
    , toList
    ) where

import Edge
import Point
import Transition

import Prelude hiding (lookup)
import qualified Data.List as L hiding (lookup)
import qualified Data.Map as M

type Key = ((Int, Int), (Int, Int))
newtype Edgeset = Edgeset (M.Map Key Edge)

----------------------
-- Public functions --
----------------------

empty :: Edgeset
empty = Edgeset M.empty

unify :: Edgeset -> Edge -> Edgeset
unify set e@(Edge p1 p2 trans) = case (lookup' key set) of
    Nothing                -> insert' key e set
    Just (Edge _ _ trans') -> insert' key newEdge set
        where newEdge = Edge p1 p2 (trans <|> trans')
    where key = keyFromEdge e

unifyAll :: Foldable t => Edgeset -> t Edge -> Edgeset
unifyAll set edges = L.foldl unify set edges

foldl :: (a -> Edge -> a) -> a -> Edgeset -> a
foldl f seed (Edgeset m) = M.foldl f seed m

fromList :: [Edge] -> Edgeset
fromList edges = L.foldl unify empty edges

toList :: Edgeset -> [Edge]
toList set = Edgeset.foldl (\listAcc e -> e:listAcc) [] set

lookup :: Point -> Point -> Edgeset -> Maybe Edge
lookup p1 p2 (Edgeset m) = M.lookup key m
    where key = keyFromPoints p1 p2

insert :: Edge -> Edgeset -> Edgeset
insert e (Edgeset m) = Edgeset (M.insert key e m)
    where key = keyFromEdge e

-----------------------
-- Private functions --
-----------------------

lookup' key (Edgeset m) = M.lookup key m

insert' key e (Edgeset m) = Edgeset (M.insert key e m)

keyFromPoints (Point x1 y1 _) (Point x2 y2 _) = ((x1, y1), (x2, y2))

keyFromEdge (Edge (Point x1 y1 _) (Point x2 y2 _) _) = ((x1, y1), (x2, y2))
