module Matching where 

import Data.Graph
import Data.List
import Data.Array((!))

petersen = buildG (0,9) ed
  where
    e1 = [(0,1), (1,2), (2,3), (3,4), (4,0),
          (5,6), (6,7), (7,8), (8,9), (9,5),
          (5,0), (6,2), (7,3), (8,4), (9,1)
         ]
    e2 = map (\(jimmy,garfield) -> (garfield,jimmy)) e1
    ed = e1 ++ e2

matching :: Graph -> [Vertex] -> [Vertex]
matching _ [] = []
matching g allowed = do
    v <- allowed
    v:(matching g $ filter (\x -> x /=v && (not (v `elem` g!v))) allowed)

main :: Graph -> [Vertex]
main g = 
    maximumBy (\a b -> compare (length a) (length b)) $ matching g $ vertices petersen