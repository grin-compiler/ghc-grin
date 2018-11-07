{-# LANGUAGE CPP #-}

-- see https://github.com/haskell/stm/pull/19
--
-- Test-case contributed by Alexey Kuleshevich <alexey@kukeshevi.ch>
--
-- This bug is observable in all versions with TBQueue from `stm-2.4` to
-- `stm-2.4.5.1` inclusive.

module Issue17 (main) where

import           Control.Concurrent.STM
import           Test.HUnit.Base        (assertBool, assertEqual)

main :: IO ()
main = do
  -- New queue capacity is set to 0
  queueIO <- newTBQueueIO 0
  assertNoCapacityTBQueue queueIO

  -- Same as above, except created within STM
  queueSTM <- atomically $ newTBQueue 0
  assertNoCapacityTBQueue queueSTM

#if !MIN_VERSION_stm(2,5,0)
  -- NB: below are expected failures

  -- New queue capacity is set to a negative numer
  queueIO' <- newTBQueueIO (-1 :: Int)
  assertNoCapacityTBQueue queueIO'

  -- Same as above, except created within STM and different negative number
  queueSTM' <- atomically $ newTBQueue (minBound :: Int)
  assertNoCapacityTBQueue queueSTM'
#endif

assertNoCapacityTBQueue :: TBQueue Int -> IO ()
assertNoCapacityTBQueue queue = do
  assertEmptyTBQueue queue
  assertFullTBQueue queue

  -- Attempt to write into the queue.
  eValWrite <- atomically $ orElse (fmap Left (writeTBQueue queue 217))
                                   (fmap Right (tryReadTBQueue queue))
  assertEqual "Expected queue with no capacity: writeTBQueue" eValWrite (Right Nothing)
  eValUnGet <- atomically $ orElse (fmap Left (unGetTBQueue queue 218))
                                   (fmap Right (tryReadTBQueue queue))
  assertEqual "Expected queue with no capacity: unGetTBQueue" eValUnGet (Right Nothing)

  -- Make sure that attempt to write didn't affect the queue
  assertEmptyTBQueue queue
  assertFullTBQueue queue


assertEmptyTBQueue :: TBQueue Int -> IO ()
assertEmptyTBQueue queue = do
  atomically (isEmptyTBQueue queue) >>=
    assertBool "Expected empty: isEmptyTBQueue should return True"

  atomically (tryReadTBQueue queue) >>=
    assertEqual "Expected empty: tryReadTBQueue should return Nothing" Nothing

  atomically (tryPeekTBQueue queue) >>=
    assertEqual "Expected empty: tryPeekTBQueue should return Nothing" Nothing

  atomically (flushTBQueue queue) >>=
    assertEqual "Expected empty: flushTBQueue should return []" []


assertFullTBQueue :: TBQueue Int -> IO ()
assertFullTBQueue queue = do
  atomically (isFullTBQueue queue) >>=
    assertBool "Expected full: isFullTBQueue shoule return True"
