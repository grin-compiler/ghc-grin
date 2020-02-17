{-# LANGUAGE CPP #-}

{- NB: This one fails for GHC < 7.6 which had a bug exposed via
       nested uses of `orElse` in `stmCommitNestedTransaction`

This was fixed in GHC via
 f184d9caffa09750ef6a374a7987b9213d6db28e
-}

module Stm064 (main) where

import           Control.Concurrent.STM
import           Control.Monad          (unless)

main :: IO ()
#if __GLASGOW_HASKELL__ >= 706 && !defined(GHC_7_6_1)
main = do
  x <- atomically $ do
         t <- newTVar (1 :: Integer)
         writeTVar t 2
         ((readTVar t >> retry) `orElse` return ()) `orElse` return ()
         readTVar t

  unless (x == 2) $
    fail (show x)
#else
main = putStrLn "Warning: test disabled for GHC < 7.6"
#endif
