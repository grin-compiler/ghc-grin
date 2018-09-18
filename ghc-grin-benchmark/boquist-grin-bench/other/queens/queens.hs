module Main(main) where

-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif

main = print (solve 12)

solve :: Int -> Int
solve nq = length (generate nq nq)

generate :: Int -> Int -> [[Int]]
generate nq 0 = [[]]
generate nq n = concatMap (add_one nq) (generate nq (n-1))

add_one :: Int -> [Int] -> [[Int]]
add_one nq xs = filter ok (map (:xs) (upto 1 nq))

ok :: [Int] -> Bool
ok []     = True
ok (x:xs) = safe x 1 xs

safe :: Int -> Int -> [Int] -> Bool
safe x d []     = True
safe x d (y:ys) = (x /= y) && (x /= y+d) && (x /= y-d) && safe x (d+1) ys

upto :: Int -> Int -> [Int]
upto m n = if m > n then
                []
            else
                m : upto (m+1) n
