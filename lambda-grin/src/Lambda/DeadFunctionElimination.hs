{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.DeadFunctionElimination where

-- NOTE: only after lambda lifting and whole program availablity

import Data.Functor.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable

import Lambda.Syntax

liveFunctionSet :: Set Name -> Exp -> Set Name
liveFunctionSet funNames = cata folder where
  folder = \case
    AppF n args | Set.member n funNames -> mconcat $ Set.singleton n : args
    VarF n      | Set.member n funNames -> Set.singleton n
    e           -> Data.Foldable.fold e

deadFunctionElimination :: Exp -> Exp
deadFunctionElimination prog@(Program defs) =
  let newProg       = Program [def | def@(Def n _ _) <- defs, Set.member n liveDefNames]
      allDefNames   = Set.fromList [n | Def n _ _ <- defs]
      liveDefNames  = liveFunctionSet allDefNames prog <> Set.singleton ":Main.main"
  in if allDefNames /= liveDefNames
    then deadFunctionElimination newProg
    else newProg
