module Dot (
      writeDot
    , toDot
    ) where

import AdjacencyMap
import Edge
import Edgeset
import Grid
import Map
import MapSquare
import Point
import Row

import qualified Data.Map as M
import qualified Data.Vector as V

----------------------
-- Public functions --
----------------------

-- Turns a Pokemon Mu map into a Graphviz-compliant graph definition, and
-- writes it to a file. This file can then be used (e.g.) to generate a PDF
-- with a nice visualization of the graph of the map.
writeDot :: FilePath -> Map -> IO ()
writeDot filename map = writeFile filename $ toDot map 

-- Turns a Pokemon Mu map into a Graphviz-compliant graph definition. If
-- written to a file, this definition can then be used (e.g.) to generate a PDF
-- with a nice visualization of the graph of the map.
toDot :: Map -> String
toDot (Map _ _ points outgoingEdges _) =
    let dotEdges = do
        row <- Grid.toList points
        point@(Point x y _) <- Row.toList row
        let node = pointToDot point
        node:(outgoingEdgesToDot outgoingEdges point)
    in unlines $ concat [["digraph map {"], dotEdges, ["}"]]

----------------------
-- Helper functions --
----------------------

-- Convert outgoing edges at a point to a list of dotfile definitions of those
-- edges.
outgoingEdgesToDot :: AdjacencyMap -> Point -> [String]
outgoingEdgesToDot outgoing point = do
    let edges = outgoing AdjacencyMap.! point
    edge <- Edgeset.toList edges
    return $ edgeToDot edge

-- Convert edge to a dotfile representation of that edge.
edgeToDot :: Edge -> String
edgeToDot (Edge (Point x1 y1 sqr1) (Point x2 y2 sqr2) t) =
    concat ["    ", pointId1, " -> ", pointId2, " [", label, color, "];"]
    where pointId1 = concat ["\"(", show x1, ",", show y1, ")\""]
          pointId2 = concat ["\"(", show x2, ",", show y2, ")\""]
          label    = concat ["label=\"", show t, "\""]
          color    = " fontcolor=blue"

-- Creates a dotfile definition of a node (e.g., generates a position, size,
-- color, etc., for the node when we render it with Graphviz). 
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
