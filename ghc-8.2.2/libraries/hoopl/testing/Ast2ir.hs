{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Ast2ir (astToIR, IdLabelMap) where


import           Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as H ((<*>))
import           Control.Monad
import qualified Data.Map       as M

import           Control.Applicative as AP (Applicative(..))

import qualified Ast as A
import qualified IR  as I

-- For the most part, the translation from an abstract-syntax trees to a graph
-- is straightforward. The one interesting complication is the translation from
-- the AST representation of labels (String) to the graph representation of
-- labels (Label).
-- To keep the mapping from (String -> Label) consistent, we use a LabelMapM monad with
-- the following operation:
labelFor :: String -> LabelMapM Label
getBody  :: forall n. Graph n C C   -> LabelMapM (Graph n C C)
run      :: LabelMapM a -> I.M (IdLabelMap, a)

-- We proceed with the translation from AST to IR; the implementation of the monad
-- is at the end of this file.

astToIR :: A.Proc -> I.M (IdLabelMap, I.Proc)
astToIR (A.Proc {A.name = n, A.args = as, A.body = b}) = run $
  do entry <- getEntry b
     body  <- toBody   b
     return $ I.Proc { I.name  = n, I.args = as, I.body = body, I.entry = entry }



getEntry :: [A.Block] -> LabelMapM Label
getEntry [] = error "Parsed procedures should not be empty"
getEntry (b : _) = labelFor $ A.first b

toBody :: [A.Block] -> LabelMapM (Graph I.Insn C C)
toBody bs =
  do g <- foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map toBlock bs)
     getBody g

toBlock :: A.Block -> LabelMapM (Graph I.Insn C C)
toBlock (A.Block { A.first = f, A.mids = ms, A.last = l }) =
  do f'  <- toFirst f
     ms' <- mapM toMid ms
     l'  <- toLast l
     return $ mkFirst f' H.<*> mkMiddles ms' H.<*> mkLast l'

toFirst :: A.Lbl -> LabelMapM (I.Insn C O)
toFirst = liftM I.Label . labelFor

toMid :: A.Insn -> LabelMapM (I.Insn O O)
toMid (A.Assign v e) = return $ I.Assign v e
toMid (A.Store  a e) = return $ I.Store  a e

toLast :: A.Control -> LabelMapM (I.Insn O C)
toLast (A.Branch l)   = labelFor l >>= return . I.Branch
toLast (A.Cond e t f) = labelFor t >>= \t' ->
                        labelFor f >>= \f' -> return (I.Cond e t' f')
toLast (A.Call rs f as l) = labelFor l >>= return . I.Call rs f as
toLast (A.Return es)      = return $ I.Return es


--------------------------------------------------------------------------------
-- The LabelMapM monad
--------------------------------------------------------------------------------

type IdLabelMap = M.Map String Label
data LabelMapM a = LabelMapM (IdLabelMap -> I.M (IdLabelMap, a))

instance Monad LabelMapM where
  return = AP.pure
  LabelMapM f1 >>= k = LabelMapM (\m -> do (m', x) <- f1 m
                                           let (LabelMapM f2) = k x
                                           f2 m')

instance Functor LabelMapM where
  fmap = liftM

instance Applicative LabelMapM where
  pure x = LabelMapM (\m -> return (m, x))
  (<*>) = ap

labelFor l = LabelMapM f
  where f m = case M.lookup l m of
                Just l' -> return (m, l')
                Nothing -> do l' <- freshLabel
                              let m' = M.insert l l' m
                              return (m', l')

getBody graph = LabelMapM f
  where f m = return (m, graph)

run (LabelMapM f) = f M.empty -- >>=  return -- . snd
