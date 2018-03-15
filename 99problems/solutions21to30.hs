module Solutions21to30 where


-- problem 21
-- insert at index
insertAt (lx) v 0 = 
    v:lx
insertAt (l:lx) v 1 = 
    l:v:lx

insertAt (l:lx) v i = 
    l:(insertAt lx v (i-1))

-- problem 22
-- range
range x y = [x..y]
