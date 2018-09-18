{-# LANGUAGE CPP, GADTs, RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

{- Exposing some internals to GHC -}
module Compiler.Hoopl.GHC
  ( uniqueToInt
  , uniqueToLbl, lblToUnique
  , getFuel, setFuel
  , bodyToBlockMap, bodyOfBlockMap
  )
where

import Compiler.Hoopl.Fuel
import Compiler.Hoopl.Graph
import Compiler.Hoopl.Label
import Compiler.Hoopl.Unique

-- Converts Body to a map of closed/closed blocks.
-- It should better be a constant-time operation
-- as GHC is counting on it.
bodyToBlockMap :: Body' block n -> LabelMap (block n C C)
bodyToBlockMap (Body bodyMap) = bodyMap

bodyOfBlockMap :: LabelMap (block n C C) -> Body' block n
bodyOfBlockMap = Body
