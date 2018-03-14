module Solutions11to20 where

-- problem 11
data Compressed a = Multiple Int a | Single a

instance (Show a) => Show (Compressed a) where
    show (Multiple c v) = "(" ++ show c ++ " " ++ show v ++ ")"
    show (Single v) = show v

encodeModified l = 
    r
    where
        folder e (sum, previous, count)
            | e == previous = (sum, previous, count + 1)
            | e /= previous && count == 1 = ((Single previous):sum, e, 1)
            | otherwise = ((Multiple count previous):sum, e, 1)
        (res, previous, count) = foldr folder ([], '-', 0) l
        r = init $ if count == 1 then (Single previous):res else (Multiple count previous):res

-- problem 12
-- decode the above
decode l = 
    helper l
    where
        helper ((Single v):xs)  =
            v:(helper xs)
        helper ((Multiple c v):xs) = 
            (take c (repeat v)) ++ (helper xs)
        helper [] =
            [] 


-- problem 13
-- encode
-- see in previous solutions



-- problem 14 
-- duplicate

duplicate l = 
    foldr folder [] l
    where 
        folder e acc =
            e:e:acc

-- problem 15
-- repliacte list elements n times

replicateList l n = 
    foldr folder [] l
    where 
        folder e acc = 
            (take n (repeat e)) ++ acc 

-- problem 16
-- drop every nth element
dropNth l n = 
    foldr folder ([],0) l
    where
        folder e (acc, c)
            | c == n = (acc, 0)
            | otherwise = (e:acc, c+1)


-- problem 17
-- split list

splitList (x:xs) n 0 =
    ([x], xs)

splitList (x:xs) n c = 
        (x:before, after)
    where
        (before, after) = splitList xs n (c-1)


-- problem 18
-- slice list
sliceList l s e = 
        take (e-s) $ drop s l
    

-- problem 19
-- rotate list
rotateN l n = 
    helper [] l n
    where
        helper start rest 0 = 
            rest ++ start
        helper start (r:rest) c = 
            helper (start ++ [r]) rest (c-1)    

-- problem 20
-- remove kth element
removeKth l k =
    helper l k
    where 
        helper (lh:lt) 0 = 
            (lh, lt)
        helper (lh:lt) c = 
            (e, lh:tail)
            where
                (e, tail) = helper lt (c-1)
