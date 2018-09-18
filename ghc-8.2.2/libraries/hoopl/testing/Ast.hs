{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Ast (Proc(..), Block(..), Insn(..), Control(..), Lbl, showProc) where

import Expr
import PP

-- | A procedure has a name, a sequence of arguments, and a body,
--   which is a sequence of basic blocks. The procedure entry
--   is the first block in the body.
data Proc = Proc { name :: String, args :: [Var], body :: [Block] } deriving Eq

-- | A block consists of a label, a sequence of instructions,
--   and a control-transfer instruction.
data Block = Block { first :: Lbl, mids :: [Insn], last :: Control } deriving Eq

-- | An instruction is an assignment to a variable or a store to memory.
data Insn = Assign Var  Expr
          | Store  Expr Expr deriving (Eq)

-- | Control transfers are branches (unconditional and conditional),
--   call, and return.
--   The Call instruction takes several parameters: variables to get
--   values returned from the call, the name of the function,
--   arguments to the function, and the label for the successor
--   of the function call.
data Control = Branch Lbl
             | Cond   Expr   Lbl    Lbl
             | Call   [Var]  String [Expr] Lbl
             | Return [Expr] deriving (Eq)

-- | Labels are represented as strings in an AST.
type Lbl = String



showProc :: Proc -> String
showProc (Proc { name = n, args = as, body = blks})
  = n ++ tuple as ++ graph
  where
    graph  = foldl (\p b -> p ++ "\n" ++ show b) (" {") blks ++ "\n}\n"

instance Show Block where
  show (Block f m l) = (foldl (\p e -> p ++ "\n" ++ show e) (f++":") m) ++ "\n" ++ show l

instance Show Insn where
  show (Assign v e)       = ind $ v ++ " = " ++ show e
  show (Store addr e)     = ind $ "m[" ++ show addr ++ "] = " ++ show e

instance Show Control where
  show (Branch lbl)       = ind $ "goto " ++ lbl
  show (Cond e t f)       =
    ind $ "if " ++ show e ++ " then goto " ++ t ++ " else goto " ++ f
  show (Call ress f cargs successor) =
    ind $ tuple ress ++ " = " ++ f ++ tuple (map show cargs) ++ " goto " ++ successor
  show (Return      rargs) = ind $ "ret " ++ tuple (map show rargs)

ind :: String -> String
ind x = "  " ++ x

{-
instance Show Value where
  show (B b) = show b
  show (I i) = show i
-}
