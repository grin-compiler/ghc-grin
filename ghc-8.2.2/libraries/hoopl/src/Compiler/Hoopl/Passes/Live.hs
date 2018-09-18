{-# LANGUAGE CPP, ScopedTypeVariables, RankNTypes, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Compiler.Hoopl.Passes.Live 
  ( NodeWithVars(..), AssignmentNode(..)
  , liveLattice, liveness, -- deadAsstElim
  ) 
where

import Data.Maybe
import qualified Data.Set as S

import Compiler.Hoopl

class HooplNode n => NodeWithVars n where
  data Var    n :: * -- ^ Variable or machine register.  Unequal variables don't alias.
  data VarSet n :: *
  foldVarsUsed :: forall e x a . (Var n -> a -> a) -> n e x -> a -> a
  foldVarsDefd :: forall e x a . (Var n -> a -> a) -> n e x -> a -> a
  killsAllVars :: forall e x . n e x -> Bool
  emptyVarSet       :: VarSet n
  unitVarSet        :: Var n -> VarSet n
  insertVarSet      :: Var n -> VarSet n -> VarSet n
  mkVarSet          :: [Var n] -> VarSet n
  unionVarSets      :: VarSet n -> VarSet n -> VarSet n
  unionManyVarSets  :: [VarSet n] -> VarSet n
  minusVarSet       :: VarSet n -> VarSet n -> VarSet n
  memberVarSet      :: Var n -> VarSet n -> Bool
  varSetElems       :: VarSet n -> [Var n]
  nullVarSet        :: VarSet n -> Bool
  varSetSize        :: VarSet n -> Int
  delFromVarSet     :: Var n -> VarSet n -> VarSet n
  delListFromVarSet :: [Var n] -> VarSet n -> VarSet n
  foldVarSet        :: (Var n -> b -> b) -> b -> VarSet n -> b -- ^ like Data.Set
  filterVarSet      :: (Var n -> Bool) -> VarSet n -> VarSet n
  intersectVarSets  :: VarSet n -> VarSet n -> VarSet n

{-
  unitVarSet x     = insertVarSet x emptyVarSet
  mkVarSet         = foldr insertVarSet emptyVarSet
  unionManyVarSets = foldr unionVarSets emptyVarSet
  delListFromVarSet= flip (foldr delFromVarSet)
-}

class NodeWithVars n => AssignmentNode n where
  isVarAssign :: n O O -> Maybe (VarSet n) -- ^ Returns 'Just xs' if /all/ the node
                                           -- does is assign to the given variables

type Live n = WithTop (VarSet n)

liveLattice :: forall n . NodeWithVars n => DataflowLattice (Live n)
liveLattice = addTop lat
  where lat :: DataflowLattice (VarSet n)
        lat = DataflowLattice
                { fact_name       = "Live variables"
                , fact_bot        = empty
                , fact_extend     = add
                , fact_do_logging = False
                }
        empty :: VarSet n
        empty = (emptyVarSet :: VarSet n)
        add :: JoinFun (VarSet n)
        add _ (OldFact old) (NewFact new) = (change, j)
          where j = new `unionVarSets` old
                change = error "type troubles"
                -- change = changeIf $ varSetSize j > varSetSize old

liveness :: NodeWithVars n => BwdTransfer n (VarSet n)
liveness = mkBTransfer first mid last
  where first f = gen_kill f
        mid   m = gen_kill m
        last  l = gen_kill l . unionManyVarSets . successorFacts l

gen_kill :: NodeWithVars n => n e x -> VarSet n -> VarSet n
gen_kill n = gen n . kill n . if killsAllVars n then const emptyVarSet else id


-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen, kill :: NodeWithVars n => n e x -> VarSet n -> VarSet n
gen  = foldVarsUsed insertVarSet
kill = foldVarsDefd delFromVarSet
     
{-
deadAsstElim :: AssignmentNode n => BwdRewrite n (VarSet n)
deadAsstElim = shallowBwdRw (noRewriteMono, dead, noRewriteMono)
  where dead n live
             | Just xs <- isVarAssign n =
                 if nullVarSet (xs `intersectVarSets` live) then Nothing
                 else Just emptyGraph
             | otherwise = Nothing
-}
