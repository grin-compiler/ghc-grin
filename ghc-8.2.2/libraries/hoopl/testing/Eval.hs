{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns , FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances,  FlexibleContexts #-}

module Eval (evalProg, ErrorM) where

import Control.Monad.Except
import qualified Data.Map    as M
import Prelude hiding (succ)

import EvalMonad
import Compiler.Hoopl
import IR

-- Evaluation functions
evalProg :: EvalTarget v => [Proc] -> [v] -> String -> [v] -> ErrorM (State v, [v])
evalProg procs vs main args = runProg procs vs $ evalProc main args

evalProc :: EvalTarget v => String -> [v] -> EvalM v [v]
evalProc proc_name actuals =
  do event $ CallEvt proc_name actuals
     proc <- get_proc proc_name
     evalProc' proc actuals
evalProc' :: EvalTarget v => Proc -> [v] -> EvalM v [v]
evalProc' (Proc {name=_, args, body, entry}) actuals =
  if length args == length actuals then
    evalBody (M.fromList $ zip args actuals) body entry
  else throwError $ "Param/actual mismatch: " ++ show args ++ " = " ++ show actuals

-- Responsible for allocating and deallocating its own stack frame.
evalBody :: EvalTarget v => VarEnv v -> Graph Insn C C -> Label -> EvalM v [v]
evalBody vars graph entry = inNewFrame vars graph $ get_block entry >>= evalB

evalB :: forall v . EvalTarget v => Block Insn C C -> EvalM v [v]
evalB b = foldBlockNodesF3 (lift evalF, lift evalM, lift evalL) b $ return ()
  where
    lift :: forall e x y . (Insn e x -> EvalM v y) -> Insn e x -> EvalM v () -> EvalM v y
    lift f n z = z >> f n


evalF :: EvalTarget v => Insn C O -> EvalM v ()
evalF (Label _) = return ()

evalM :: EvalTarget v => Insn O O -> EvalM v ()
evalM (Assign var e) =
  do v_e <- eval e
     set_var var v_e
evalM (Store addr e) =
  do v_addr <- eval addr >>= toAddr
     v_e    <- eval e
     -- StoreEvt recorded in set_heap
     set_heap v_addr v_e

evalL :: EvalTarget v => Insn O C -> EvalM v [v]
evalL (Branch bid) =
  do b <- get_block bid
     evalB b
evalL (Cond e t f) =
  do v_e <- eval e >>= toBool
     evalL $ Branch $ if v_e then t else f
evalL (Call ress f args succ) =
  do v_args <- mapM eval args
     -- event is recorded in evalProc
     f_ress <- evalProc f v_args
     if length ress == length f_ress then return ()
      else throwError $ "function " ++ f ++ " returned unexpected # of args"
     _ <- mapM (uncurry set_var) $ zip ress f_ress
     evalL $ Branch succ
evalL (Return es) =
  do vs <- mapM eval es
     event $ RetEvt vs
     return vs

class Show v => EvalTarget v where
  toAddr :: v    -> EvalM v Integer
  toBool :: v    -> EvalM v Bool
  eval   :: Expr -> EvalM v v

instance EvalTarget Value where
  toAddr (I i) = return i
  toAddr (B _) = throwError "conversion to address failed"
  toBool (B b) = return b
  toBool (I _) = throwError "conversion to bool failed"
  eval (Lit (Int  i)) = return $ I i
  eval (Lit (Bool b)) = return $ B b
  eval (Var var) = get_var var
  eval (Load addr) =
    do v_addr <- eval addr >>= toAddr
       get_heap v_addr
  eval (Binop bop e1 e2) =
    do v1 <- eval e1
       v2 <- eval e2
       liftBinOp bop v1 v2
    where
      liftBinOp = liftOp
        where liftOp Add  = i (+)
              liftOp Sub  = i (-)
              liftOp Mul  = i (*)
              liftOp Div  = i div
              liftOp Eq   = b (==)
              liftOp Ne   = b (/=)
              liftOp Gt   = b (>)
              liftOp Lt   = b (<)
              liftOp Gte  = b (>=)
              liftOp Lte  = b (<=)
              i = liftX I fromI
              b = liftX B fromB

              liftX :: Monad m => (a -> b) -> (b -> m a) -> (a -> a -> a) -> b -> b -> m b
              liftX up dwn = \ op x y -> do v_x <- dwn x
                                            v_y <- dwn y
                                            return $ up $ op v_x v_y
              fromI (I x) = return x
              fromI (B _) = throwError "fromI: got a B"

              fromB (I _) = throwError "fromB: got an I"
              fromB (B x) = return x

-- I'm under no delusion that the following example is useful,
-- but it demonstrates how the evaluator can use a new kind
-- of evaluator.
instance EvalTarget Integer where
  toAddr i = return i
  toBool i = return $ i /= 0
  eval (Lit (Int i)) = return i
  eval (Lit (Bool True)) = return 1
  eval (Lit (Bool False)) = return 0
  eval (Var var) = get_var var
  eval (Load addr) =
    do v_addr <- eval addr >>= toAddr
       get_heap v_addr
  eval (Binop bop e1 e2) =
    do v1 <- eval e1
       v2 <- eval e2
       return $ liftBinOp bop v1 v2
    where
      liftBinOp = liftOp
        where liftOp Add  = i (+)
              liftOp Sub  = i (-)
              liftOp Mul  = i (*)
              liftOp Div  = i div
              liftOp Eq   = b (==)
              liftOp Ne   = b (/=)
              liftOp Gt   = b (>)
              liftOp Lt   = b (<)
              liftOp Gte  = b (>=)
              liftOp Lte  = b (<=)
              i = id
              b opr x y = if opr x y then 1 else 0


-- Symbolic evaluation.
-- Hard questions:
--  - how do we get heap addresses?
--  - how do we get conditionals?
--  - how do we compare symbolic expressions?
data Sym = L  Lit
         | In Integer -- In x indicates a value on entry to the program
         | Ld Sym
         | BO BinOp Sym Sym
  deriving Show
-- sym_vsupply :: [Sym]
-- sym_vsupply = [In n | n <- [0..]]

instance EvalTarget Sym where
  toAddr _ = undefined
  toBool _ = undefined
  eval (Lit l) = return $ L l
  eval (Var var) = get_var var
  eval (Load addr) =
    do v_addr <- eval addr >>= toAddr
       get_heap v_addr
  eval (Binop bop e1 e2) =
    do v1 <- eval e1
       v2 <- eval e2
       return $ BO bop v1 v2
