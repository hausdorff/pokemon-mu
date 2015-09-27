module Map where

import Prelude hiding (Left, Right)
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as S
import qualified Data.Vector as V

--------------------
-- Map data types --
--------------------

data MapSquare = Entrance | Exit | Wall | Grass | Ground
    deriving (Eq, Ord)

data Point = Point Int Int MapSquare
    deriving (Eq, Ord)

data Transition = Up | Down | Left | Right | A | B | Select | Start
    deriving (Show)

data Edge = Edge Point Point Transition

type Grid = V.Vector (V.Vector Point)

type AdjacencyMap = M.Map Point [Edge]

data Map = Map { points        :: Grid
               , outgoingEdges :: AdjacencyMap
               , incomingEdges :: AdjacencyMap
               }

------------------------------
-- Show/Read code for types --
------------------------------

instance Show MapSquare where
    show Entrance = "S"
    show Exit     = "E"
    show Wall     = "#"
    show Ground   = " "

instance Read MapSquare where
    readsPrec _ "S" = [(Entrance, "")]
    readsPrec _ "E" = [(Exit, "")]
    readsPrec _ "#" = [(Wall, "")]
    readsPrec _ " " = [(Ground, "")]

instance Show Point where
    show point = show $ sqr point

instance Show Map where
    show (Map rows _ _) = unlines $ V.toList (V.map convertRow rows)
        where convertRow = map pointToChar . V.toList

-----------------------
-- Utility functions --
-----------------------

makeOutgoingEdges :: Grid -> AdjacencyMap
makeOutgoingEdges points = V.foldl processRow M.empty points
    where processRow acc row     = V.foldl processPoint acc row
          processPoint map point = M.insert point edges map
              where edges = (neighborEdges points (x point) (y point))

makeIncomingEdges :: Grid -> AdjacencyMap
makeIncomingEdges points = M.empty
 
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

makeEdge from to direction = case (sqr from, sqr to) of
    (Wall, _) -> Nothing
    (_, Wall) -> return $ Edge from from direction
    otherwise -> return $ Edge from to direction

neighborEdges :: Grid -> Int -> Int -> [Edge]
neighborEdges points x y = Maybe.catMaybes neighbors
    where directions = [Up, Down, Left, Right]
          neighbors  = map (getEdge points x y) directions

----------------------
-- Helper functions --
----------------------

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

revindexed :: [a] -> [(Int, a)]
revindexed xs = zip indexes xs
    where indexes = reverse $ take (length xs) [0..]

charToSqr :: Char -> MapSquare
charToSqr c = read [c]

sqrToChar :: MapSquare -> Char
sqrToChar s = case (show s) of
    [c] -> c

pointToChar :: Point -> Char
pointToChar p = case (show p) of
    [c] -> c


x (Point x _ _) = x
y (Point _ y _) = y
sqr (Point _ _ sqr) = sqr

---------------
-- Map tests --
---------------

makeMap rows = Map { points      = as2dVect
                 , outgoingEdges = makeOutgoingEdges as2dVect
                 , incomingEdges = makeIncomingEdges as2dVect
                 }
    where mkPt x y c           = Point x y (charToSqr c)
          rowToPoints (y, row) = V.fromList $ map (\(x, c) -> mkPt x y c) (indexed row)
          as2dVect             = V.fromList $ map rowToPoints (revindexed rows)

testM = makeMap ["####",
                 "# E#",
                 "#S #",
                 "####"]