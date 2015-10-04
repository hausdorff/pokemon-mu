module Edge (
      Edge(..)
    , makeEdge
    , unify
    )where

import MapSquare
import Point
import Transition

data Edge = Edge Point Point Transition

makeEdge :: Point -> Point -> Transition -> Maybe Edge
makeEdge from to direction = case (sqr from, sqr to) of
    (Wall, _) -> Nothing
    (_, Wall) -> return $ Edge from from direction
    otherwise -> return $ Edge from to direction

unify :: Edge -> Edge -> Edge
unify (Edge p1 p2 trans) (Edge _ _ trans') = Edge p1 p2 (trans <|> trans')

sqr (Point _ _ sqr) = sqr