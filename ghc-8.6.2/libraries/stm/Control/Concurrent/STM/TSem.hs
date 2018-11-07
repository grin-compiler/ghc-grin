-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TSem
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- 'TSem': transactional semaphores.
--
-- @since 2.4.2
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module Control.Concurrent.STM.TSem
  ( TSem
  , newTSem

  , waitTSem

  , signalTSem
  , signalTSemN
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Numeric.Natural

-- | 'TSem' is a transactional semaphore.  It holds a certain number
-- of units, and units may be acquired or released by 'waitTSem' and
-- 'signalTSem' respectively.  When the 'TSem' is empty, 'waitTSem'
-- blocks.
--
-- Note that 'TSem' has no concept of fairness, and there is no
-- guarantee that threads blocked in `waitTSem` will be unblocked in
-- the same order; in fact they will all be unblocked at the same time
-- and will fight over the 'TSem'.  Hence 'TSem' is not suitable if
-- you expect there to be a high number of threads contending for the
-- resource.  However, like other STM abstractions, 'TSem' is
-- composable.
--
-- @since 2.4.2
newtype TSem = TSem (TVar Integer)
  deriving (Eq, Typeable)

-- | Construct new 'TSem' with an initial counter value.
--
-- A positive initial counter value denotes availability of
-- units 'waitTSem' can acquire.
--
-- The initial counter value can be negative which denotes a resource
-- \"debt\" that requires a respective amount of 'signalTSem'
-- operations to counter-balance.
--
-- @since 2.4.2
newTSem :: Integer -> STM TSem
newTSem i = fmap TSem (newTVar $! i)

-- NOTE: we can't expose a good `TSem -> STM Int' operation as blocked
-- 'waitTSem' aren't reliably reflected in a negative counter value.

-- | Wait on 'TSem' (aka __P__ operation).
--
-- This operation acquires a unit from the semaphore (i.e. decreases
-- the internal counter) and blocks (via 'retry') if no units are
-- available (i.e. if the counter is /not/ positive).
--
-- @since 2.4.2
waitTSem :: TSem -> STM ()
waitTSem (TSem t) = do
  i <- readTVar t
  when (i <= 0) retry
  writeTVar t $! (i-1)


-- Alternatively, the implementation could block (via 'retry') when
-- the next increment would overflow, i.e. testing for 'maxBound'

-- | Signal a 'TSem' (aka __V__ operation).
--
-- This operation adds\/releases a unit back to the semaphore
-- (i.e. increments the internal counter).
--
-- @since 2.4.2
signalTSem :: TSem -> STM ()
signalTSem (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+1


-- | Multi-signal a 'TSem'
--
-- This operation adds\/releases multiple units back to the semaphore
-- (i.e. increments the internal counter).
--
-- > signalTSem == signalTSemN 1
--
-- @since 2.4.5
signalTSemN :: Natural -> TSem -> STM ()
signalTSemN 0 _ = return ()
signalTSemN 1 s = signalTSem s
signalTSemN n (TSem t) = do
  i <- readTVar t
  writeTVar t $! i+(toInteger n)
