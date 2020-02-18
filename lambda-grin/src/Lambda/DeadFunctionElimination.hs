{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.DeadFunctionElimination where

-- NOTE: only after lambda lifting and whole program availablity

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Lambda.Syntax2

deadFunctionElimination :: Program -> Program
deadFunctionElimination (Program exts cons sdata defs) = Program liveExts liveCons liveSData liveDefs where

  liveSData = [sd  | sd <- sdata, Set.member (sName sd) liveNames]
  liveExts  = [ext | ext <- exts, Set.member (eName ext) liveNames]
  liveDefs  = [def | def@(Def name _ _) <- defs, Set.member name liveSet]
  liveCons  = cons -- TODO

  liveNames = cata collectAll $ Program [] [] [] liveDefs -- collect all live names

  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  lookupDef :: Name -> Maybe Def
  lookupDef name = Map.lookup name defMap

  liveSet :: Set Name
  liveSet = fst $ until (\(live, visited) -> live == visited) visit (Set.singleton ":Main.main", mempty)

  visit :: (Set Name, Set Name) -> (Set Name, Set Name)
  visit (live, visited) = (mappend live seen, mappend visited toVisit) where
    toVisit = Set.difference live visited
    seen    = foldMap (maybe mempty (cata collect) . lookupDef) toVisit

  collect :: ExpF (Set Name) -> Set Name
  collect = \case
    AppF name args  -> Set.fromList [n | n <- name : args, Map.member n defMap]
    ConF _ args     -> Set.fromList [n | n <- args, Map.member n defMap]
    VarF name       | Map.member name defMap -> Set.singleton name
    CaseF name _    | Map.member name defMap -> Set.singleton name
    exp -> Data.Foldable.fold exp

  collectAll :: ExpF (Set Name) -> Set Name
  collectAll = \case
    AppF name args  -> Set.fromList $ name : args
    ConF _ args     -> Set.fromList args
    CaseF name _    -> Set.singleton name
    VarF name       -> Set.singleton name
    exp -> Data.Foldable.fold exp
