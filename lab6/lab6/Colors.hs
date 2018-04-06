{-# OPTIONS -Wall #-}
module Colors where

import Data.Sequence(Seq,adjust,index)
import qualified Data.Sequence as Sequence
import Data.Graph
-- graf jest tablicą list
import Data.Array((!))
import Data.List

-- listy dupuszczalnych kolorow
type Color = Int
type AllowedColors = Seq [Color]

assignColor :: Graph -> Vertex -> Color -> AllowedColors -> AllowedColors
assignColor gr vertex color allowed = foldl' (\allow v -> adjust removeColor v allow) allowed vToRemColor
  where
    vToRemColor = gr!vertex
    removeColor = filter (/=color)

colorGraph :: Graph -> Color -> [[Color]]
colorGraph graph numColors = colorGraph' (vertices graph) allowedColors
  where
    allowedColors = Sequence.replicate (length $ vertices graph) $ [1..numColors]
    colorGraph' :: [Vertex] -> AllowedColors -> [[Color]]
    colorGraph' [] _ = return []
    colorGraph' (vertex:vs) allowed = do
      color <- allowed `index` vertex
      coloring <- colorGraph' vs (assignColor graph vertex color allowed)
      return $ color:coloring

petersen :: Graph
petersen = buildG (0,9) ed
  where
    e1 = [(0,1), (1,2), (2,3), (3,4), (4,0),
          (5,6), (6,7), (7,8), (8,9), (9,5),
          (5,0), (6,2), (7,3), (8,4), (9,1)
         ]
    e2 = map (\(jimmy,garfield) -> (garfield,jimmy)) e1
    ed = e1 ++ e2
