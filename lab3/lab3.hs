module Lab3 where

import Data.List (foldl')
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
isDescending [] = (True, 0)
isDescending (x:xs) = 
    foldl' folder (True, x) xs
    where
        folder (isSorted, last) e =
            if isSorted && last >= e then (True, e)
            else (False, e)

-- zad 6 
sumNums l1 l2 =     
    acc
    where
        len1 = length l1
        len2 = length l2
        alligned1 = if len1 < len2 then (take (len2 - len1) $ repeat 0) ++ l1 else l1
        alligned2 = if len2 < len1 then (take (len1 - len2) $ repeat 0) ++ l2 else l2
        r1 = reverse alligned1
        r2 = reverse alligned2
        zipped = zip r1 r2
        folder (val, acc) (e1,e2) =
            let sum = e1 + e2 + acc 
            in
                if sum >= 10 then ((sum - 10):val, 1)
                else (sum:val, 0)
        (acc,_) = foldl folder ([], 0) zipped

-- zad 7
eratostenes n =
    etatostenes' [2..n] [2..n]
    where
        etatostenes' [] r = r
        etatostenes' (x:xs) result =
            x:(etatostenes'  av rs)
            where 
                av = filter (\e -> (mod e x) /= 0) xs
                rs = filter (\e -> (mod e x) /= 0) result

-- zad 8 
data Numb = Zero | Succ Numb

instance (Show a) => Show (Numb a) where
    show Zero  = "0"
    show Succ a = "0" ++ show a


-- laborki punktowane:
-- typ danych
-- type klasy
-- operacje
-- unikanie wycieków pamieci
-- rekurencja ogonkowa
-- operacje na listach
