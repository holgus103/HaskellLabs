-- zad 4
 module SortedList where 

data Element a = Nil | Value a (Element a)

instance (Show a) => Show (Element a) where
    show (Nil) = ""
    show (Value val next) = show val ++ " " ++ show next

add :: (Ord a) => Element a -> a -> Element a
add (Nil) val = 
    Value val Nil
    
add e@(Value v next) val
    | val <= v = Value val e 
    | val > v = Value v $ add next val 

avg list = 
    sum/count
    where
        loopOverList Nil = (0, 0)
        loopOverList (Value v n) =
            (c+1, s + v)
            where
                (c, s) = loopOverList n
        (count, sum) = loopOverList list


fltr :: (Ord a) => (a -> Bool) -> Element a -> Element a

fltr f Nil = Nil
fltr f (Value x n) = 
    if f x then add (fltr f n) x else fltr f n