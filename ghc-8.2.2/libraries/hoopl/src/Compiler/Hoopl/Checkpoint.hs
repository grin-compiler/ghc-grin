{-# LANGUAGE CPP, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Checkpoint
  ( CheckpointMonad(..)
  )
where

-- | Obeys the following law:
-- for all @m@ 
-- @
--    do { s <- checkpoint; m; restart s } == return ()
-- @
class Monad m => CheckpointMonad m where
  type Checkpoint m
  checkpoint :: m (Checkpoint m)
  restart    :: Checkpoint m -> m () 

