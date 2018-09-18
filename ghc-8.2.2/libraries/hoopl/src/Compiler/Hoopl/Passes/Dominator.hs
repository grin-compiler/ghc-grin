{-# LANGUAGE CPP, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
#if __GLASGOW_HASKELL__ >= 723
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Passes.Dominator
  ( Doms, DPath(..), domPath, domEntry, domLattice, extendDom
  , DominatorNode(..), DominatorTree(..), tree
  , immediateDominators
  , domPass
  )
where

import Data.Maybe
import qualified Data.Set as Set

import Compiler.Hoopl


type Doms = WithBot DPath
-- ^ List of labels, extended with a standard bottom element

-- | The fact that goes into the entry of a dominator analysis: the first node
-- is dominated only by the entry point, which is represented by the empty list
-- of labels.
domEntry :: Doms
domEntry = PElem (DPath [])

newtype DPath = DPath [Label]
  -- ^ represents part of the domination relation: each label
  -- in a list is dominated by all its successors.  This is a newtype only so
  -- we can give it a fancy Show instance.

instance Show DPath where
  show (DPath ls) = concat (foldr (\l path -> show l : " -> " : path) ["entry"] ls)

domPath :: Doms -> [Label]
domPath Bot = [] -- lies: an unreachable node appears to be dominated by the entry
domPath (PElem (DPath ls)) = ls

extendDom :: Label -> DPath -> DPath
extendDom l (DPath ls) = DPath (l:ls)

domLattice :: DataflowLattice Doms
domLattice = addPoints "dominators" extend

extend :: JoinFun DPath
extend _ (OldFact (DPath l)) (NewFact (DPath l')) =
                                (changeIf (l `lengthDiffers` j), DPath j)
    where lx = filter (\elem -> Set.member elem common) l
          rx = filter (\elem -> Set.member elem common) l'
          common = Set.intersection (Set.fromList l) (Set.fromList l')
          j = [x | (x, y) <- zip lx rx, x == y]

          lengthDiffers [] [] = False
          lengthDiffers (_:xs) (_:ys) = lengthDiffers xs ys
          lengthDiffers [] (_:_) = True
          lengthDiffers (_:_) [] = True



-- | Dominator pass
domPass :: (NonLocal n, Monad m) => FwdPass m n Doms
domPass = FwdPass domLattice (mkFTransfer3 first (const id) distributeFact) noFwdRewrite
  where first n = fmap (extendDom $ entryLabel n)

----------------------------------------------------------------

data DominatorNode = Entry | Labelled Label
data DominatorTree = Dominates DominatorNode [DominatorTree]
-- ^ This data structure is a *rose tree* in which each node may have
--  arbitrarily many children.  Each node dominates all its descendants.

-- | Map from a FactBase for dominator lists into a
-- dominator tree.  
tree :: [(Label, Doms)] -> DominatorTree
tree facts = Dominates Entry $ merge $ map reverse $ map mkList facts
   -- This code has been lightly tested.  The key insight is this: to
   -- find lists that all have the same head, convert from a list of
   -- lists to a finite map, in 'children'.  Then, to convert from the
   -- finite map to list of dominator trees, use the invariant that
   -- each key dominates all the lists of values.
  where merge lists = mapTree $ children $ filter (not . null) lists
        children = foldl addList noFacts
        addList :: FactBase [[Label]] -> [Label] -> FactBase [[Label]]
        addList map (x:xs) = mapInsert x (xs:existing) map
            where existing = fromMaybe [] $ lookupFact x map
        addList _ [] = error "this can't happen"
        mapTree :: FactBase [[Label]] -> [DominatorTree]
        mapTree map = [Dominates (Labelled x) (merge lists) |
                                                    (x, lists) <- mapToList map]
        mkList (l, doms) = l : domPath doms


instance Show DominatorTree where
  show = tree2dot

-- | Given a dominator tree, produce a string representation, in the
-- input language of dot, that will enable dot to produce a
-- visualization of the tree.  For more info about dot see
-- http://www.graphviz.org.

tree2dot :: DominatorTree -> String
tree2dot t = concat $ "digraph {\n" : dot t ["}\n"]
  where
    dot :: DominatorTree -> [String] -> [String]
    dot (Dominates root trees) = 
                   (dotnode root :) . outedges trees . flip (foldl subtree) trees
      where outedges [] = id
            outedges (Dominates n _ : ts) =
                \s -> "  " : show root : " -> " : show n : "\n" : outedges ts s
            dotnode Entry = "  entryNode [shape=plaintext, label=\"entry\"]\n"
            dotnode (Labelled l) = "  " ++ show l ++ "\n"
            subtree = flip dot

instance Show DominatorNode where
  show Entry = "entryNode"
  show (Labelled l) = show l

----------------------------------------------------------------

-- | Takes FactBase from dominator analysis and returns a map from each 
-- label to its immediate dominator, if any
immediateDominators :: FactBase Doms -> LabelMap Label
immediateDominators = mapFoldWithKey add mapEmpty
    where add l (PElem (DPath (idom:_))) = mapInsert l idom 
          add _ _ = id

