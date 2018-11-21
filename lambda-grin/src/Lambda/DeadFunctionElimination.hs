{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.DeadFunctionElimination where

-- NOTE: only after lambda lifting and whole program availablity

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Lambda.Syntax

deadFunctionElimination :: Program -> Program
deadFunctionElimination (Program defs) = Program [def | def@(Def name _ _) <- defs, Set.member name liveDefs] where
  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  lookupDef :: Name -> Maybe Def
  lookupDef name = Map.lookup name defMap

  liveDefs :: Set Name
  liveDefs = fst $ until (\(live, visited) -> live == visited) visit (Set.singleton {- ":Main.main" -} "Main.main", mempty)

  visit :: (Set Name, Set Name) -> (Set Name, Set Name)
  visit (live, visited) = (mappend live seen, mappend visited toVisit) where
    toVisit = Set.difference live visited
    seen    = foldMap (maybe mempty (cata collect) . lookupDef) toVisit

  collect :: ExpF (Set Name) -> Set Name
  collect = \case
    AppF name args  | Map.member name defMap  -> mconcat $ Set.singleton name : args
    VarF name       | Map.member name defMap  -> Set.singleton name
    exp -> Data.Foldable.fold exp
