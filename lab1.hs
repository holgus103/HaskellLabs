module Lab1 where

main :: IO()
main =
    putStrLn "asd"


-- zad 1    
absIf :: Integer -> Integer
absIf l = 
    if l < 0 then -l else l

absGuards l
    | l < 0 = -l
    | otherwise = l

-- zad 2
clipIf min max x = 
    if min > x then min
    else if max < x then max
    else x

clip l u = (mi . ma)
    where 
        mi = min u
        ma = max l

 
-- zad 3
sinTaylor x n = 
    foldl folder (0, 0) [1..n]
    where 
        folder (sum, prev) e = 
            (sum + current, current)
            where
            current = helper x e prev 
        helper x 1 0 = 
            -1 / 6 * x * x * x
        helper x n prev =
           prev * (-1) * x * x / (2 * n + 1) / (2*n)
       
-- zad 4
funMax :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) 
funMax f1 f2 x = 
    max (f1 x) (f2 x)

-- zad 5
xyzzy x = 
    negate (ceiling (abs (cos x))) 

xyzzydol x = 
    negate $ ceiling $ abs $ cos x

xyzzycom = 
    negate . ceiling . abs . cos
    
-- zad 6 
mySqrt :: Double -> Double -> Double
mySqrt x eps = 
    helper x
    where
        helper val =
            if abs(val - newVal) < eps then newVal
            else  (helper newVal)
            where
                newVal = val - (val*val - x)/(2 * val)
            
                

-- sinTaylorLooped x n = 
--     looper n
--     where 
--         helper acc _ 0 = 
--             acc
--         helper acc f i =
         
--            where
--            newF = f * (-1) * x * x / (2 * n + 1) / (2*n)

        
    