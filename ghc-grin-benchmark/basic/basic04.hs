{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "_prim_int_print" prim_int_print :: Int -> IO ()

data IntList
  = Nil
  | Cons Int IntList

main = prim_int_print (xsum 0 (upto 1 10))

upto :: Int -> Int -> IntList
upto m n = if m > n then Nil else Cons m (upto (m+1) n)

xsum :: Int -> IntList -> Int
xsum n Nil     = n
xsum n (Cons x xs) = xsum (n+x) xs
