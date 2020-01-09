{-# LANGUAGE LambdaCase #-}
module Lambda.Util where

import Data.Functor.Foldable
import qualified Data.Set as Set
import qualified Data.Foldable

import Transformations.Names hiding (mkNameEnv)
import Transformations.Util hiding (foldNameDefExpF)
import Lambda.Syntax

mkNameEnv :: Exp -> NameEnv
mkNameEnv exp = NameEnv mempty (cata folder exp) where
  folder e = foldNameDefExpF Set.singleton e `mappend` Data.Foldable.fold e

foldLocalNameDefExp :: (Monoid m) => (Name -> m) -> Exp -> m
foldLocalNameDefExp f = foldLocalNameDefExpF f . project

foldLocalNameDefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldLocalNameDefExpF f = \case
  DefF _ args _ -> mconcat $ map f args
  ProgramF{}    -> mempty
  e             -> foldNameDefExpF f e

foldNameDefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameDefExpF f = \case
  ProgramF _ sdata _    -> mconcat $ map (f . sName) sdata
  DefF n args _         -> mconcat $ map f $ n : args
  LetF bs _             -> mconcat $ map (f . fst) bs
  LetRecF bs _          -> mconcat $ map (f . fst) bs
  LetSF bs _            -> mconcat $ map (f . fst) bs
  AltF (NodePat _ bs) _ -> mconcat $ map f bs
  ClosureF _ bs _       -> mconcat $ map f bs
  _                     -> mempty

mapNameExp :: (Name -> Name) -> Exp -> Exp
mapNameExp f = \case
  Program e s d -> Program e [sd {sName = f $ sName sd} | sd <- s] d
  Def n args e  -> Def (f n) (map f args) e
  exp           -> mapLocalNameExp f exp

mapLocalNameExp :: (Name -> Name) -> Exp -> Exp
mapLocalNameExp f = \case
  -- def
  Def n args e          -> Def n (map f args) e -- NOTE: do not touch top level function name
  LetRec bs e           -> LetRec [(f n, a) | (n,a) <- bs] e
  Let bs e              -> Let  [(f n, a) | (n,a) <- bs] e
  LetS bs e             -> LetS [(f n, a) | (n,a) <- bs] e
  Alt (NodePat n bs) e  -> Alt (NodePat n (map f bs)) e
  Closure vs bs e       -> Closure (map f vs) (map f bs) e
  -- use
  App n l               -> App (f n) l
  Var p n               -> Var p (f n)
  -- other
  exp                   -> exp

isAtom :: Exp -> Bool
isAtom = \case
  Var{} -> True
  Lit{} -> True
  _     -> False
