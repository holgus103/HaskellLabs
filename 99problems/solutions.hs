module Solutions where
-- problem 1
-- last list element

getLast [] = 
    error "The list is empty!"

getLast [x] = 
    x

getLast (_:xs) = 
    getLast xs


-- problem 2
-- last but one

getForLast [] =
    error "List is too short"

getForLast [x] = 
    getForLast []

getForLast [x,y] =
    x
   
getForLast (_:xs) = 
    getForLast xs


-- problem 3 
-- get nth element
nth [] n = 
    error "Out of bounds"

nth (x:_) 1 = x
nth (x:xs) n = 
    nth xs (n-1)

-- problem 4
-- get length
len [] = 0
len (_:xs) =
    1 + (length xs)

-- problem 5
-- reverse list

rev [] n = n
rev [x] n = x:n

rev (x:xs) acc = 
    rev xs (x:acc)

-- problem 6
-- palindromes

isPalindrome i = 
    helper i $ reverse i
    where
        helper [] [] = True
        helper _ [] = False
        helper [] _ = False
        helper (x:xs) (y:ys)
            | x == y = helper xs ys
            | otherwise =  False

-- problem 7
-- flatmap non-recursive

flatMap x =
    [i | j <- x, i <- j]

-- flatmap recursive
data NestedList a = Elem a | List [NestedList a]

flatMapR (Elem e) =
     [e]

flatMapR (List l) = 
    concatMap flatMapR l

-- sample call: flatMapR (List [Elem 1, List [Elem 1, List [Elem 1]]])

-- problem 8
-- compress

compress :: [Char] -> [Char]
compress l =
    fst $ foldr folder ([], '-') l
    where
        folder e (compressed, last) = 
            if e == last then  (compressed, last)
            else (e:compressed, e) 


-- problem 9
-- sublist packing

pack l = 
    current:res
    where 
        folder e (sum, last, current) 
            | e == last = (sum, last, e:current)
            | otherwise = (current:sum, e, [e])
        (res, _, current) =  foldr folder ([], '-', []) l


-- problem 10 
-- kawaii compression

encode l = 
    (count, last):res
    where 
        folder e (acc, last, count)
            | e == last = (acc, last, count + 1)
            | otherwise = ((count, last):acc, e, 1)
        (res, last, count) = foldr folder ([], '-', 0) l