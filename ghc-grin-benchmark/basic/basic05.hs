{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "_prim_int_print" prim_int_print :: Int -> IO ()

a :: [Int]
a = [1..1000]

b :: Int
b = sum a

main = prim_int_print b
