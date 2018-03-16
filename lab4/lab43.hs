module Lab43 where

shorten :: [Char] -> Int -> [Char]
shorten text n = 
    res ++ show (count - 1) ++ [(last text)]
    wheree
        folder (s, i, c) e =
            if i > 0 then (s ++ [e], i - 1, 0)
            else (s, 0, c + 1)
        (res, _, count) = foldl folder ([], n, 0) text

shorten2 :: [Char] -> Int -> [Char]
shorten2 text n = 
    helper text n 0
    where
        helper [t] i s =
            show s ++ [t] 
        helper (t:tx) i s = 
            if i > 0 then 
                t:(helper tx (i - 1) 0)
            else 
                (helper tx i (s+1))

            
-- maxout l1 l2 l3 l4 = 
--     zipWith (\x y z q ->)
