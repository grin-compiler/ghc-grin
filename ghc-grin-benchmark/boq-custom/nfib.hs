module Main(main) where

import RTS

main = print_int (nfib 35)

nfib :: Int -> Int
nfib n = if n <= 1 then
             1
         else
             1 + nfib (n-1) + nfib (n-2)

-- 32 -> 7049155
-- 35 -> 29860703
