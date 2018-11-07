-- STM stress test

module Stm052 (main) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad      (mapM_, when)
import           Data.Array
import           Data.List
import           Foreign
import           Foreign.C
import           GHC.Conc
import           GHC.Conc           (unsafeIOToSTM)
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Random

-- | The number of array elements
n_elems :: Int
n_elems = 20

-- | The number of threads swapping elements
n_threads :: Int
n_threads = 2

-- | The number of swaps for each thread to perform
iterations :: Int
iterations = 20000

type Elements = Array Int (TVar Int)

thread :: TVar Int -> Elements -> IO ()
thread done elements = loop iterations
 where loop 0 = atomically $ do x <- readTVar done; writeTVar done (x+1)
       loop n = do
          i1 <- randomRIO (1,n_elems)
          i2 <- randomRIO (1,n_elems)
          let e1 = elements ! i1
          let e2 = elements ! i2
          atomically $ do
            e1_v <- readTVar e1
            e2_v <- readTVar e2
            writeTVar e1 e2_v
            writeTVar e2 e1_v
          loop (n-1)

await_end :: TVar Int -> IO ()
await_end done = atomically $ do x <- readTVar done
                                 if (x == n_threads)  then return () else retry

main :: IO ()
main = do
  _ <- Foreign.newStablePtr stdout
  setStdGen (read "526454551 6356")
  let init_vals = [1..n_elems] -- take n_elems
  tvars <- atomically $ mapM newTVar init_vals
  let elements = listArray (1,n_elems) tvars
  done <- atomically (newTVar 0)
  _ <- sequence [ forkIO (thread done elements) | _id <- [1..n_threads] ]
  await_end done
  fin_vals <- mapM (\t -> atomically $ readTVar t) (elems elements)

  when (sort fin_vals /= init_vals) $ do
    putStr("Before: ")
    mapM_ (\v -> putStr ((show v) ++ " " )) init_vals
    putStr("\nAfter: ")
    mapM_ (\v -> putStr ((show v) ++ " " )) (sort fin_vals)
    putStr("\n")
    fail "mismatch"
