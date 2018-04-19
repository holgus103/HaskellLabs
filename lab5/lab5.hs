module Lab5 where

import Data.Char
import Data.List
import System.IO.Error
import System.IO
import Text.Read
import Control.Applicative
import Control.Monad
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



-- zad 2 - monads
calculatorM :: IO Int
calculatorM = do
    c <- readOpM
    a <- readIntM
    b <- readIntM
    return $ operationM c a b 
    

readOpM :: IO String
readOpM =
    getLine

readIntM :: IO Int
readIntM = 
    getLine >>= (\x -> return $ read x)

operationM :: String -> Int -> Int -> Int
operationM c a b
    | c == "+" = a + b
    | c == "-" = a - b
    | c == "*" = a * b



-- zad 2 - applicative

operation :: String -> (Int -> Int -> Int)
operation c
    | c == "+" = (+)
    | c == "-" = (-)
    | c == "*" = (*)

calculate :: IO Int
calculate = 
    operation <$> readOpM <*> readIntM <*> readIntM

-- zad 3 
grep :: String -> IO [()]
grep i = 
    catchIOError (sequence $ repeat process_) (\_ -> return [()])
    where 
        process_ :: IO ()
        process_ = process i 

process :: String -> IO ()
process i = do
    line <- getLine
    if isPrefixOf i line then putStrLn line else return ()

grepRdo :: String -> IO ()
grepRdo i = do
    line <- getLine
    if isPrefixOf i line then putStrLn line else return ()
    grepRdo i

grepRop :: String -> IO ()
grepRop i = 
    getLine >>= (\l -> (if isPrefixOf i l then putStrLn l else return ()) >> grepRop i )

grepAlt :: String -> IO [()]
grepAlt i = 
    many $ process i 

-- zad 4
countTextLines :: String -> IO (Int, Int)
countTextLines path =
    withFile path ReadMode (\f -> process f (0,0))
    where

        processLine :: Handle -> (Int, Int) -> IO (Int, Int)
        processLine f (lines, bytes) = do
            line <- hGetLine f
            process f (lines + 1, bytes +  length line)

        process :: Handle -> (Int, Int) -> IO (Int, Int)
        process f (lines, bytes) = do
            catchIOError (processLine f (lines, bytes)) (\_-> return (lines, bytes))

-- zad 5
grepWithFiles :: String -> String -> IO ()
grepWithFiles path i = 
    withFile path ReadMode _main
    where 
        _main :: Handle -> IO ()
        _main f = 
            catchIOError (void $ sequence $ repeat process_) (\_ -> return ())
            where
                process_ :: IO ()
                process_ = do
                    line <- hGetLine f
                    if isPrefixOf i line then putStrLn line else return ()


-- zad 6
exercise6 :: IO [Double]
exercise6 = 
    getLine >>= process
    where 
        process :: String -> IO [Double]
        process line = 
            case parseList line of 
                Nothing -> return [fromIntegral $ length line]
                Just x -> return [fromIntegral $maximum x, fromIntegral $ minimum x, (fromIntegral $ sum x) / (fromIntegral $ length x)]

        

parseList ::  [Char] -> Maybe [Int]
parseList l =
    if any func parsed then Nothing else sequence parsed
    where
        numbers = splitList l
        parsed :: [Maybe Int]
        parsed = map readMaybe numbers
        func Nothing = True
        func _ = False 

splitList :: [Char] ->  [String]
splitList l = 
    element:list
    where
        folder :: ([String], [Char]) -> Char -> ([String], [Char])
        folder (output, current) e = 
            if e == ',' then (current:output, "")
            else (output, current ++ [e])
        (list, element) = foldl folder ([], "") l


-- zad 7 

sortMain :: [Int] -> [Int]
sortMain l = do {   
    first <- l;
    mySort [first] $ delete first l
}


mySort :: [Int] -> [Int] -> [Int]

mySort l [] = l;  

mySort (l:lx) res = do {
    first <- res;
    if first <= l then mySort (first:l:lx) (delete first res)
    else []

}



        