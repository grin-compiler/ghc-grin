{-# LANGUAGE ForeignFunctionInterface #-}
--import Hello
import Data.Word

foreign import ccall "print_int64" print_int64 :: Word64 -> IO ()

{-# NOINLINE test #-}
test :: Int -> IO ()
test x = do
  print_int64 $ fromIntegral x

main = do
  putStrLn "hello!" --f
  test 1
  test 2
