module FibLoop (fib')
  where

fibloop :: Integer -> Integer -> Integer -> Integer
fibloop n n_1 n_2
  | n <= 1 = n_1 + n_2
  | otherwise = fibloop (seq (n_1+n_2) (n-1)) (n_1+n_2) n_1
  -- | otherwise = fibloop (n-1) (n_1+n_2) n_1

fib' :: Integer -> Integer
fib' n = fibloop n 1 0