{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "_prim_int_print" prim_int_print :: Int -> IO ()

main = prim_int_print 1
