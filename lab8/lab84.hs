module Lab84 where

podzialLiczb :: Int -> Int -> [([Int], [Int])]
podzialLiczb a b = do
    splita <- split a
    splitb <- split b 
    return (splita, splitb)

split :: Int -> [[Int]]
split 1 = [[1]]
split i = do
    v <- [1..i]
    currentSplit <- split (i - v)
    return (v:currentSplit)

