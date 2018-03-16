module Solutions61to69 where

data Tree a = Empty | Branch a (Tree a) (Tree a)
               deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)

-- problem 61
-- count leaves
countLeaves (Branch v Empty Empty) = 1;
countLeaves (Branch v Empty left) = countLeaves left
countLeaves (Branch v right Empty) = countLeaves right
countLeaves (Branch v left right) = (countLeaves left) + (countLeaves right)

-- problem 61A
-- collect leaves
collectLeaves (Branch v Empty Empty) = [v];
collectLeaves (Branch v Empty left) = collectLeaves left
collectLeaves (Branch v right Empty) = collectLeaves right
collectLeaves (Branch v left right) = (collectLeaves left) ++ (collectLeaves right)

-- problem 62
-- collect nodes
collectNodes (Branch v Empty Empty) = [];
collectNodes (Branch v Empty left) = v:(collectNodes left)
collectNodes (Branch v right Empty) = v:(collectNodes right)
collectNodes (Branch v left right) = v:(collectNodes left) ++ (collectNodes right)


-- problem 62A
-- collect nodes at level n
collectNodesAt n c (Branch v Empty Empty) = 
    if n == c then [v] else [];
collectNodesAt n c (Branch v Empty left) =
    if n == c then [v] else collectNodesAt n (c+1) left
collectNodesAt n c (Branch v right Empty) = 
    if n == c then [v] else collectNodesAt n (c+1) right
collectNodesAt n c (Branch v left right) =
    if n == c then [v] else (collectNodesAt n (c+1) left) ++ (collectNodesAt n (c+1) right)

