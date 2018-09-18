{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, PatternGuards #-}
module Simplify (simplify) where

import Control.Monad
import Compiler.Hoopl
import IR
import OptSupport

type Node = Insn


-- @ start cprop.tex

--------------------------------------------------
-- Simplification ("constant folding")
simplify :: forall m f. FuelMonad m => FwdRewrite m Node f
simplify = deepFwdRw simp
 where
  simp :: forall e x. Node e x -> f -> m (Maybe (Graph Node e x))
  simp node _ = return $ liftM insnToG $ s_node node
  s_node :: Node e x -> Maybe (Node e x)
  s_node (Cond (Lit (Bool b)) t f)
    = Just $ Branch (if b then t else f)
  s_node n = (mapEN . mapEE) s_exp n
  s_exp (Binop Add (Lit (Int n1)) (Lit (Int n2)))
    = Just $ Lit $ Int $ n1 + n2
    -- ... more cases for constant folding
-- @ end cprop.tex
  s_exp (Binop Div _lhs (Lit (Int 0)))
    = Nothing
  s_exp (Binop opr e1 e2)
    | (Just op, Lit (Int i1), Lit (Int i2)) <- (intOp opr, e1, e2) =
        Just $ Lit $ Int  $ op i1 i2
    | (Just op, Lit (Int i1), Lit (Int i2)) <- (cmpOp opr, e1, e2) =
        Just $ Lit $ Bool $ op i1 i2
  s_exp _ = Nothing
  intOp Add = Just (+)
  intOp Sub = Just (-)
  intOp Mul = Just (*)
  intOp Div = Just div
  intOp _   = Nothing
  cmpOp Eq  = Just (==)
  cmpOp Ne  = Just (/=)
  cmpOp Gt  = Just (>)
  cmpOp Lt  = Just (<)
  cmpOp Gte = Just (>=)
  cmpOp Lte = Just (<=)
  cmpOp _   = Nothing
