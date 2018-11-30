module Main(main) where

import RTS

main = print_int (sum (sieve (upto 2 50000)))

upto :: Int -> Int -> [Int]
upto m n = if m > n then [] else m : upto (m+1) n

xfilter :: Int -> [Int] -> [Int]
xfilter y l = case l of
                []     -> []
                (x:xs) -> if x `rem` y == 0
                          then xfilter y xs
                          else x : xfilter y xs
sieve :: [Int] -> [Int]
sieve l = case l of
             []     -> []
             (x:xs) -> x : sieve (xfilter x xs)
