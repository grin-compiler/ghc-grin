{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Ir2ast (irToAst) where

import           Compiler.Hoopl
import           Control.Monad
import qualified Data.Map       as M

import qualified Ast as A
import qualified IR  as I
import Control.Monad.Reader

type Rm = Reader (M.Map Label A.Lbl)

invertMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invertMap m = foldl (\p (k,v) ->
                      if M.member v p
                      then error $ "irrefutable error in invertMap, the values are not unique"
                      else M.insert v k p
                    ) M.empty (M.toList m)


strLabelFor :: Label -> Rm String
strLabelFor l = do { mp <- ask
                   ; case M.lookup l mp of
                     Just x -> return x
                     Nothing -> return $ "_hoopl_generated_label_" ++ (show l)
                   }

irToAst :: M.Map String Label -> I.Proc -> A.Proc
irToAst mp (I.Proc {I.name = n, I.args = as, I.body = graph, I.entry = entry }) =
  runReader (do { body <- fromGraph entry graph
                ; return $ A.Proc { A.name = n, A.args = as, A.body = body }
                }) (invertMap mp)

fromGraph :: Label -> Graph I.Insn C C -> Rm [A.Block]
fromGraph entry g = let entryNode = gUnitOC (BlockOC BNil (I.Branch entry))
                        blks = reverse $ postorder_dfs (gSplice entryNode g)
                    in foldM (\p blk -> do { ablk <- fromBlock blk ()
                                           ; return (ablk:p)
                                           }) [] blks



type instance IndexedCO C () (Rm (A.Lbl, [A.Insn])) = ()
type instance IndexedCO C (Rm A.Block) (Rm (A.Lbl, [A.Insn])) = Rm A.Block

fromBlock :: Block I.Insn C C -> () -> Rm A.Block
fromBlock blk = foldBlockNodesF3 (fromIrInstCO, fromIrInstOO, fromIrInstOC) blk


fromIrInstCO :: I.Insn C O -> () -> Rm (A.Lbl, [A.Insn])
fromIrInstCO inst _ = case inst of
  I.Label l -> strLabelFor l >>= \x -> return (x, [])


fromIrInstOO :: I.Insn O O -> Rm (A.Lbl, [A.Insn]) -> Rm (A.Lbl, [A.Insn])
fromIrInstOO inst p = case inst of
  I.Assign v e -> do { (sl, insts) <- p
                     ; return (sl, (A.Assign v e):insts)
                     }
  I.Store a e -> do { (sl, insts) <- p
                    ; return (sl, (A.Store a e):insts)
                    }


fromIrInstOC :: I.Insn e x -> Rm (A.Lbl, [A.Insn]) -> Rm A.Block
fromIrInstOC inst p = case inst of
  I.Branch tl -> do { (l, insts) <- p
                    ; stl <- strLabelFor tl
                    ; return $ A.Block {A.first = l, A.mids = reverse insts
                                       , A.last = A.Branch stl}
                    }
  I.Cond e tl fl -> do { (l, insts)<- p
                       ; stl <- strLabelFor tl
                       ; sfl <- strLabelFor fl
                       ; return $ A.Block {A.first = l, A.mids = reverse insts
                                          , A.last = A.Cond e stl sfl}
                       }
  I.Call vars name exps el -> do { (l, insts) <- p
                                 ; tel <- strLabelFor el
                                 ; return $ A.Block {A.first = l, A.mids = reverse insts
                                                    , A.last = A.Call vars name exps tel}
                                 }
  I.Return exps -> do { (l, insts) <- p
                      ; return $ A.Block {A.first = l, A.mids = reverse insts
                                         , A.last = A.Return exps}
                      }
