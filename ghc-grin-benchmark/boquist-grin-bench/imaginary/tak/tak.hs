-- code of unknown provenance (partain 95/01/25)
module Main(main) where

#ifdef __HBCC__
#define print xprint
#endif

tak :: Int -> Int -> Int -> Int

tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

main = print (tak 24 16 8)
