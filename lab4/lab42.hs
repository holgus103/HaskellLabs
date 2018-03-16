module Lab42 where
    
seqA l k 0 =
    3
    
seqA l k 2 =
    1

seqA l k 1 =
    k

seqA l k n = 
    (seqA l k (n-1)) - ((*) l  $! seqA l k (n-3))



seqAList l k = 
    [3, k, 1] ++ (helper l k 1 k 3)
    where
        helper l k p1 p2 p3 = 
            newVal:(helper l k newVal p1 p2)
            where
                newVal = (p1 - (l * p3))

    
