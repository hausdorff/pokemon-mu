module Edgeset (
      Edgeset
    , empty
    , unify
    , fromList
    , toList
    ) where

import Edge
import Point
import Transition

import qualified Data.List as L
import qualified Data.Map as M

type Key = ((Int, Int), (Int, Int))
type Edgeset = M.Map Key Edge

empty :: Edgeset
empty = M.empty

unify :: Edgeset -> Edge -> Edgeset
unify set e@(Edge p1 p2 trans) = case (M.lookup key set) of
    Nothing -> M.insert key e set
    Just (Edge _ _ trans') -> M.insert key newEdge set
        where newEdge = Edge p1 p2 (trans <|> trans')
    where key = makeKey e

unifyAll :: Foldable t => Edgeset -> t Edge -> Edgeset
unifyAll set edges = L.foldl unify set edges

fromList :: [Edge] -> Edgeset
fromList edges = L.foldl unify empty edges

toList :: Edgeset -> [Edge]
toList set = M.foldl (\listAcc e -> e:listAcc) [] set

makeKey (Edge (Point x1 y1 _) (Point x2 y2 _) _) = ((x1, y1), (x2, y2))
