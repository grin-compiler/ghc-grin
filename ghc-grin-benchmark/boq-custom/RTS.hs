{-# LANGUAGE ForeignFunctionInterface #-}

module RTS where

foreign import ccall "_prim_int_print" prim_int_print :: Int -> IO ()

print_int :: Int -> IO ()
print_int = prim_int_print

print_int_list :: [Int] -> IO ()
print_int_list = mapM_ print_int
