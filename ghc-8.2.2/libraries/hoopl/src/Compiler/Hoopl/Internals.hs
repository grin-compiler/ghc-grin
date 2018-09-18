{-# LANGUAGE CPP, GADTs, RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

{- Exposing some internals to friends (e.g. GHC) -}
module Compiler.Hoopl.Internals
  ( module Compiler.Hoopl.Block
  , module Compiler.Hoopl.Graph
  , module Compiler.Hoopl.Label
  , module Compiler.Hoopl.Dataflow
  )
where

import Compiler.Hoopl.Block
import Compiler.Hoopl.Graph
import Compiler.Hoopl.Label
import Compiler.Hoopl.Dataflow
