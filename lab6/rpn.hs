module Rpn where 

import Data.List
import System.IO

calc :: [String] -> [Int] -> Int
calc [] [a] = a
calc [] _ = 0
calc (i:input) (a:b:stack)
    | i == "*" = calc input ((a * b):stack)
    | i == "+" = calc input ((a + b):stack)
    | otherwise = calc input ((read i):stack) 

calc (i:input) stack = calc input ((read i):stack)
    

     