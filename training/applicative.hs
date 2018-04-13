module Applicative where 

operationFromCharCode :: Char -> Either String (Double -> Double -> Double)
operationFromCharCode '*' = Right (*)
operationFromCharCode '+' = Right (+)
operationFromCharCode c = Left $  "Unknown opcode: "++[c]


safeLog :: Double -> Either String Double
safeLog x
  | x <=0 = Left "Log needs positive arg"
  | otherwise = Right $ log x