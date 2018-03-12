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
