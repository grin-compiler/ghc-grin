module Stm065 (main) where

import           Control.Concurrent.STM
import           Control.Monad          (unless)

main :: IO ()
main = do
  x <- atomically $ do
         r <- newTVar []
         writeTVar r [2 :: Integer]
         writeTVar r [] `orElse` return ()
         readTVar r

  unless (null x) $ do
    fail (show x)
