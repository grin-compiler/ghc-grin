{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Wrappers {-# DEPRECATED "Use only if you know what you are doing and can preserve the 'respects fuel' invariant" #-}
  ( wrapFR, wrapFR2, wrapBR, wrapBR2
  )
where

import Compiler.Hoopl.Dataflow

