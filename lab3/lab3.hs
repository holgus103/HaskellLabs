module Lab3 where
-- zad 1
-- recurrency version
contains :: Eq a => [a] -> a -> Bool 
contains [] v = False;
contains (x:xs) v =
    if v == x then True
    else contains xs v

    
-- fold version
containsFold :: Eq a => [a] -> a -> Bool
containsFold list v =
    foldl folder False list
    where 
        folder acc el
            | acc = True
            | el == v = True
            | otherwise = False


-- zad 2
getPi :: Double -> Double
getPi n =
    4*(1 + posSum + negSum)
    where    
        pos = map (\x -> 1/(4*x + 1)) [1..n]
        neg = map (\x -> -1/(4*x - 1)) [1..n]
        posSum = sum pos
        negSum = sum neg

getPiList n =
    4*(1 + posSum + negSum)
    where 
        pos = [1/(4*x+1) | x <- [1..n]]
        neg = [-1/(4*x-1) | x <- [1..n]]
        posSum = sum pos
        negSum = sum neg


-- zad 3
findLargest =
    last [x | x <- [1..100000], (mod x 3829) == 0 ]


-- zad 4
quicksort [] = []
quicksort (x:xs) =
    quicksort smaller ++ [x] ++ quicksort greater
    where
        smaller = filter (\e -> e <= x) xs
        greater = filter (\e -> e > x) xs

-- zad 5 