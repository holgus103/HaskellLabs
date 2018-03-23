module Lab5 where

import Data.Char
-- zad 1
capitalize s =      
        case all isLower s of 
            True -> Just $ map toUpper s
            False -> Nothing

unaryNumber n 
    | n > 0 = Just $ take n $ repeat 0
    | otherwise = Nothing

unaryNumberM n
    | n == 0 = Just []
    | n < 0 = Nothing
    | otherwise = (unaryNumberM (n-1)) >>= (\x -> return $ 0:x)

interleave s1 s2 = 
    concatMap f c 
    where
        f (a,b) = 
            [a] ++ [b] 
        c = zip s1 s2

capitalizeM [] = 
    Just []     
    
capitalizeM (s:sx)
    | isLower s = do        
        tail <- capitalizeM sx
        return $ (toUpper s):tail
    | otherwise = Nothing

capitalizeM2 [] = Just []
capitalizeM2 (s:sx)
    | isLower s = (capitalizeM sx) >>= (\x -> return $ (toUpper s):x) 
    | otherwise = Nothing



-- zad 2
calculator :: IO Int
calculator = do
    c <- readOp
    a <- readInt
    b <- readInt
    return $ operation c a b 
    

readOp :: IO String
readOp =
    getLine

readInt :: IO Int
readInt = 
    getLine >>= (\x -> return $ read x)

operation :: String -> Int -> Int -> Int
operation c a b
    | c == "+" = a + b
    | c == "-" = a - b
    | c == "*" = a * b

