{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Lambda.ClosureConversion where

import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set

import Transformations.Util
import Transformations.Names hiding (mkNameEnv)

import Lambda.Syntax
import Lambda.Util


type ClosM = WriterT [Def] NameM

without :: Eq a => [a] -> [a] -> [a]
without = foldr (filter . (/=)) -- Like \\ but removes all occurrences

applyTo :: Exp -> [Name] -> Exp
applyTo e []          = e
applyTo (Var n) args  = App n (map Var args)
applyTo e args        = AppExp e (map Var args)

freeVars :: Exp -> [Name] -- Grab all the unbound variables in an expression
freeVars = cata folder where
  folder = \case
    VarF v      -> [v]
    AppF n e    -> n : mconcat e
    LamF vs e   -> e `without` vs
    e           -> Data.Foldable.fold e

closConv :: [Name] -> Exp -> Exp
closConv globals = cata folder where
  folder (LamF vs e) = let vars = freeVars e `without` (globals ++ vs)
                       in Lam (vars ++ vs) e `applyTo` vars
  folder e = embed e

liftLam :: Exp -> ClosM Exp
liftLam = cataM folder where
  folder = \case
    LamF vs e -> do
      fresh <- lift $ deriveNewName "closure" -- TODO: better name
      tell [Def fresh vs e]
      pure $ Var fresh

    -- smash
    AppExpF (Var n) args
      | all isAtom args       -> pure $ App n args
    LetF l1 (Let l2 e)        -> pure $ Let (l1 ++ l2) e
    LetSF l1 (LetS l2 e)      -> pure $ LetS (l1 ++ l2) e
    e                         -> pure $ embed e

smash :: Exp -> Exp
smash = cata folder where
  folder = \case
    LamF vs (Lam vs' e) -> Lam (vs ++ vs') e
    AppExpF (Var n) args
      | all isAtom args -> App n args
    e                   -> embed e

eliminateLams :: [Name] -> Program -> Program
eliminateLams globals prg@(Program defs) = Program $ defs' ++ newDefs where
  (Program defs', newDefs) = evalState (runWriterT . liftLam{- . smash . closConv (defNames ++ globals)-} $ prg) (mkNameEnv prg)
  defNames = [n | Def n _ _ <- defs]
