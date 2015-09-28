module Edge (
      Edge(..)
    , makeEdge
    )where

import MapSquare
import Point
import Transition

data Edge = Edge Point Point Transition

makeEdge from to direction = case (sqr from, sqr to) of
    (Wall, _) -> Nothing
    (_, Wall) -> return $ Edge from from direction
    otherwise -> return $ Edge from to direction

sqr (Point _ _ sqr) = sqr