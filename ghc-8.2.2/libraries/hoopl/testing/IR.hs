{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module IR (Proc (..), Insn (..), Expr (..), Lit (..), Value (..), BinOp(..), Var
          , showProc
          , M) where

import Prelude hiding (succ)

import Compiler.Hoopl
import Expr
import PP

type M = CheckingFuelMonad (SimpleUniqueMonad)

data Value = B Bool | I Integer deriving Eq

data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph Insn C C }

data Insn e x where
  Label  :: Label  ->                               Insn C O
  Assign :: Var    -> Expr    ->                    Insn O O
  Store  :: Expr   -> Expr    ->                    Insn O O
  Branch :: Label  ->                               Insn O C
  Cond   :: Expr   -> Label   -> Label  ->          Insn O C
  Call   :: [Var]  -> String  -> [Expr] -> Label -> Insn O C
  Return :: [Expr] ->                               Insn O C

instance NonLocal Insn where
  entryLabel (Label l)      = l
  successors (Branch l)     = [l]
  successors (Cond _ t f)   = [t, f]
  successors (Call _ _ _ l) = [l]
  successors (Return _)     = []

--------------------------------------------------------------------------------
-- Prettyprinting
--------------------------------------------------------------------------------

showProc :: Proc -> String
showProc proc = name proc ++ tuple (args proc) ++ graph
  where
    graph  = " {\n" ++ showGraph show (body proc) ++ "}\n"

instance Show (Insn e x) where
  show (Label lbl)        = show lbl ++ ":"
  show (Assign v e)       = ind $ v ++ " = " ++ show e
  show (Store addr e)     = ind $ "m[" ++ show addr ++ "] = " ++ show e
  show (Branch lbl)       = ind $ "goto " ++ show lbl
  show (Cond e t f)       =
    ind $ "if " ++ show e ++ " then goto " ++ show t ++ " else goto " ++ show f
  show (Call ress f cargs succ) =
    ind $ tuple ress ++ " = " ++ f ++ tuple (map show cargs) ++ " goto " ++ show succ
  show (Return      rargs) = ind $ "ret " ++ tuple (map show rargs)

ind :: String -> String
ind x = "  " ++ x

instance Show Value where
  show (B b) = show b
  show (I i) = show i
