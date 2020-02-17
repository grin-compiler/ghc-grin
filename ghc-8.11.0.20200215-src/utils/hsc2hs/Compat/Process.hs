{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

#if (!MIN_VERSION_process(1,6,7) \
    && defined(mingw32_HOST_OS) \
    && MIN_VERSION_process(1,5,0))
#define NEEDS_PROCESS_WORKAROUND 1
#else
#define NEEDS_PROCESS_WORKAROUND 0
#endif

#if NEEDS_PROCESS_WORKAROUND
{-# LANGUAGE InterruptibleFFI #-}
#endif

-- This module backports `waitForProcess` from version 1.6.6.1 of the process
-- library in order to fix an issue with processes calling spawn or execv not
-- being waited on correctly on Windows when using older versions of hsc2hs.
--
-- See also https://gitlab.haskell.org/ghc/ghc/issues/10731
--
-- When hsc2hs supports process 1.6.6.1 as minimum then this module can be
-- removed.
module Compat.Process (
    waitForProcess
  ) where

#if NEEDS_PROCESS_WORKAROUND
import Control.Concurrent
import Data.Maybe

import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import System.Process.Internals hiding (waitForJobCompletion)
import System.Exit      ( ExitCode(..) )

#else
import qualified System.Process as Process
import System.Process (ProcessHandle)
import System.Exit      ( ExitCode() )
#endif

-- ----------------------------------------------------------------------------
-- waitForProcess

{- | Waits for the specified process to terminate, and returns its exit code.

GHC Note: in order to call @waitForProcess@ without blocking all the
other threads in the system, you must compile the program with
@-threaded@.

(/Since: 1.2.0.0/) On Unix systems, a negative value @'ExitFailure' -/signum/@
indicates that the child was terminated by signal @/signum/@.
The signal numbers are platform-specific, so to test for a specific signal use
the constants provided by "System.Posix.Signals" in the @unix@ package.
Note: core dumps are not reported, use "System.Posix.Process" if you need this
detail.

-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
#if !NEEDS_PROCESS_WORKAROUND
waitForProcess ph = Process.waitForProcess ph
#else
waitForProcess ph = lockWaitpid $ do
  p_ <- modifyProcessHandle ph $ \p_ -> return (p_,p_)
  case p_ of
    OpenExtHandle _ job iocp ->
        maybe (ExitFailure (-1)) mkExitCode `fmap` waitForJobCompletion job iocp timeout_Infinite
      where mkExitCode code | code == 0 = ExitSuccess
                            | otherwise = ExitFailure $ fromIntegral code
    _  -> error "Only supports waiting for process jobs. Use process directly."
  where
    -- If more than one thread calls `waitpid` at a time, `waitpid` will
    -- return the exit code to one of them and (-1) to the rest of them,
    -- causing an exception to be thrown.
    -- Cf. https://github.com/haskell/process/issues/46, and
    -- https://github.com/haskell/process/pull/58 for further discussion
    lockWaitpid m = withMVar (waitpidLock ph) $ \() -> m

waitForJobCompletion :: PHANDLE
                     -> PHANDLE
                     -> CUInt
                     -> IO (Maybe CInt)
waitForJobCompletion job io timeout =
            alloca $ \p_exitCode -> do
              items <- newMVar $ []
              setter <- mkSetter (insertItem items)
              getter <- mkGetter (getItem items)
              ret <- c_waitForJobCompletion job io timeout p_exitCode setter getter
              if ret == 0
                 then Just <$> peek p_exitCode
                 else return Nothing

insertItem :: MVar [(k, v)] -> k -> v -> IO ()
insertItem env_ k v = modifyMVar_ env_ (return . ((k, v):))

getItem :: Eq k => MVar [(k, v)] -> k -> IO v
getItem env_ k = withMVar env_ (\m -> return $ fromJust $ lookup k m)

-- ----------------------------------------------------------------------------
-- Interface to C bits

type SetterDef = CUInt -> Ptr () -> IO ()
type GetterDef = CUInt -> IO (Ptr ())

foreign import ccall "wrapper"
  mkSetter :: SetterDef -> IO (FunPtr SetterDef)
foreign import ccall "wrapper"
  mkGetter :: GetterDef -> IO (FunPtr GetterDef)

foreign import ccall interruptible "__waitForJobCompletion" -- NB. safe - can block
  c_waitForJobCompletion
        :: PHANDLE
        -> PHANDLE
        -> CUInt
        -> Ptr CInt
        -> FunPtr (SetterDef)
        -> FunPtr (GetterDef)
        -> IO CInt
#endif /* NEEDS_TEMP_WORKAROUND */
