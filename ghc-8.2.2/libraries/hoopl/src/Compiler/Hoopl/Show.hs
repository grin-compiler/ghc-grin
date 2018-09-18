{-# LANGUAGE CPP, RankNTypes, GADTs, ScopedTypeVariables, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Show 
  ( showGraph, showFactBase, Showing
  )
where

import Compiler.Hoopl.Collections
import Compiler.Hoopl.Block
import Compiler.Hoopl.Graph
import Compiler.Hoopl.Label

--------------------------------------------------------------------------------
-- Prettyprinting
--------------------------------------------------------------------------------

type Showing n = forall e x . n e x -> String
 

showGraph :: forall n e x . Showing n -> Graph n e x -> String
showGraph node = g
  where g :: Graph n e x -> String
        g GNil = ""
        g (GUnit block) = b block
        g (GMany g_entry g_blocks g_exit) =
            open b g_entry ++ body g_blocks ++ open b g_exit
        body blocks = concatMap b (mapElems blocks)
        b :: forall e x . Block n e x -> String
        b (BlockCO l b1)   = node l ++ "\n" ++ b b1
        b (BlockCC l b1 n) = node l ++ "\n" ++ b b1 ++ node n ++ "\n"
        b (BlockOC   b1 n) =           b b1 ++ node n ++ "\n"
        b (BNil)          = ""
        b (BMiddle n)     = node n ++ "\n"
        b (BCat b1 b2)    = b b1   ++ b b2
        b (BSnoc b1 n)    = b b1   ++ node n ++ "\n"
        b (BCons n b1)    = node n ++ "\n" ++ b b1

open :: (a -> String) -> MaybeO z a -> String
open _ NothingO  = ""
open p (JustO n) = p n

showFactBase :: Show f => FactBase f -> String
showFactBase = show . mapToList
