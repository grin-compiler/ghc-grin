module Main(main) where

#ifdef __HBCC__
#define print xprint
#endif

main = print (nfib 35)

nfib :: Int -> Int
nfib n = if n <= 1 then
             1
         else
	     1 + nfib (n-1) + nfib (n-2)

-- 32 -> 7049155
-- 35 -> 29860703
