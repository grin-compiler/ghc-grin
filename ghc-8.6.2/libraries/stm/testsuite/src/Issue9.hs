{-# LANGUAGE CPP #-}

-- see https://github.com/haskell/stm/pull/9
--
-- Test-case contributed by Mitchell Rosen <mitchellwrosen@gmail.com>
--
-- This bug is observable in version `stm-2.4.5.0`

module Issue9 (main) where

import           Control.Concurrent.STM
import           Data.Foldable

main :: IO ()
#if MIN_VERSION_stm(2,4,5)
main = do
  -- New queue with capacity 5
  queue <- newTBQueueIO 5

  -- Fill it up with [1..5]
  for_ [1..5] $ \i ->
    atomically (writeTBQueue queue (i :: Int))

  -- Read 1
  1 <- atomically (readTBQueue queue)

  -- Flush [2..5]
  [2,3,4,5] <- atomically (flushTBQueue queue)

  -- The bug: now the queue capacity is 4, not 5.
  -- To trigger it, first fill up [1..4]...
  for_ [1..4] $ \i ->
    atomically (writeTBQueue queue i)

  -- ... then observe that writing a 5th element will fail
  -- with "thread blocked indefinitely in an STM transaction"
  atomically (writeTBQueue queue 5)
#else
-- test-case not applicable; `flushTBQueue` was only added in 2.4.5.0
main = return ()
#endif
