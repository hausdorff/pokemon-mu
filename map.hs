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

data MapSqr = Entrance | Exit | Wall | Grass | Ground
    deriving (Eq, Ord)

data Point = Point Int Int MapSqr
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

instance Show MapSqr where
    show Entrance = "S"
    show Exit     = "E"
    show Wall     = "#"
    show Ground   = " "

instance Read MapSqr where
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

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

revindexed :: [a] -> [(Int, a)]
revindexed xs = zip indexes xs
    where indexes = reverse $ take (length xs) [0..]

charToSqr :: Char -> MapSqr
charToSqr c = read [c]

sqrToChar :: MapSqr -> Char
sqrToChar s = case (show s) of
    [c] -> c

pointToChar :: Point -> Char
pointToChar p = case (show p) of
    [c] -> c


x (Point x _ _) = x
y (Point _ y _) = y
sqr (Point _ _ sqr) = sqr


-----------------
-- Graph utils --
-----------------

writeDot :: FilePath -> Map -> IO ()
writeDot filename map = writeFile filename $ toDot map 

toDot :: Map -> String
toDot (Map points outgoingEdges _) =
    let dotEdges = do
        row <- V.toList points
        point <- V.toList row
        let node = pointToDot point
        node:(outgoingEdgesToDot outgoingEdges point)
    in unlines $ concat [["digraph map {"], dotEdges, ["}"]]

-- Convert outgoing edges at a point to a list of dotfile definitions of those
-- edges.
outgoingEdgesToDot :: AdjacencyMap -> Point -> [String]
outgoingEdgesToDot outgoing point = do
    let edges = outgoing M.! point
    edge <- edges
    return $ edgeToDot edge

-- Convert edge to a dotfile representation of that edge.
edgeToDot :: Edge -> String
edgeToDot (Edge p1 p2 t) =
    concat ["    ", pointId1, " -> ", pointId2, ";"]
    where (x1, y1, sqr1) = (show $ x p1, show $ y p1, show $ sqr p1)
          (x2, y2, sqr2) = (show $ x p2, show $ y p2, show $ sqr p2)
          pointId1       = concat ["\"(", x1, ",", y1, ")\""]
          pointId2       = concat ["\"(", x2, ",", y2, ")\""]


pointToDot :: Point -> String
pointToDot point@(Point x y sqr) =
    concat ["    ", pointId, " [", shape, " ", label, " ", pos, " ", fillcolor, "];"]
    where (x', y', sqr') = (show x, show y, show sqr)
          pointId        = concat ["\"(", x', ",", y', ")\""]
          shape          = "shape=circle"
          label          = concat ["label=\"", sqr', "\\n(", x', ",", y', ")\""]
          (posx, posy)   = (show $ 2*x, show $ 2*y)
          pos            = concat ["pos=\"", posx, ",", posy, "!\""]
          fillcolor      = case sqr of
              Wall     -> "style=\"filled\" fillcolor=palegreen"
              Entrance -> "style=\"filled\" fillcolor=yellow"
              Exit     -> "style=\"filled\" fillcolor=red"
              _        -> "fillcolor=white"

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