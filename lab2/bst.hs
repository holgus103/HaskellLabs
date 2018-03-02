-- zad 3

module BST where

data Bst a = Nil | Node a (Bst a) (Bst a)
    
contains :: (Ord a) => Bst a -> a -> Bool
contains Nil _ = False
contains (Node x left right) needle
    | x == needle = True
    | x < needle = contains left needle
    | x > needle = contains right needle

add :: (Ord a) => Bst a -> a -> (Bst a, Bool)
add Nil x = (Node x Nil Nil, True)

add n@(Node x left right) y
  | x == y = (n, False)
  | x < y && not la = (n, False)
  | x < y = (Node x lp right, True)
  | x > y && not ra = (n, False)
  | x > y = (Node x left rp, True)
  where
    (lp, la) = add left y
    (rp, ra) = add right y

findSucccessor n@(Node x Nil Nil) = 
    Nil

findSuccessor n@(Node x Nil right) = 
    right

findSuccessor n@(Node x left Nil) =
    left

findSuccessor n@(Node x left right) = 
    findChild right
    where
        findChild e@(Node val Nil r) = 
            e
        findChild e@(Node val l r) = 
            findChild l


addIgn :: (Ord a) => Bst a -> a -> Bst a
addIgn tree el = fst $ add tree el


remove :: (Ord a) => Bst a -> a -> Bst a
remove n@(Node x Nil Nil) val 
    | x == val = Nil
    | otherwise = n  

    
remove (Node x left Nil) val 
    | x == val = left
    | otherwise = Node val (remove left val) Nil

remove (Node x Nil right) val 
    | x == val = right
    | otherwise = Node val Nil (remove right val) 

remove n@(Node x left right) val
    | val > x = Node x (remove left val) right
    | val < x = Node x left (remove right val)
    | val == x = Node nv left $ remove right nv
        where
            (Node nv _ _) = findSuccessor n