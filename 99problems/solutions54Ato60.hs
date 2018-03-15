module Solutions54Ato60 where

data Tree a = Leaf | Node a (Tree a) (Tree a)

tree1 = Node 'a' (Node 'b' (Node 'd' Leaf Leaf)
                               (Node 'e' Leaf Leaf))
                   (Node 'c' Leaf
                               (Node 'f' (Node 'g' Leaf Leaf)
                               Leaf))

-- isInTree tree = 
    

