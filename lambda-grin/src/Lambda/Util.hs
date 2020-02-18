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
  DefF _ args _ -> mconcat $ map (f . fst) args
  ProgramF{}    -> mempty
  e             -> foldNameDefExpF f e

foldNameDefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameDefExpF f = \case
  ProgramF _ _ sdata _    -> mconcat $ map (f . sName) sdata
  DefF n args _           -> mconcat $ f n : map (f . fst) args
  LetF bs _               -> mconcat $ map (f . fst3) bs
  LetRecF bs _            -> mconcat $ map (f . fst3) bs
  LetSF bs _              -> mconcat $ map (f . fst3) bs
  AltF a (NodePat _ bs) _ -> mconcat $ f a : map f bs
  ClosureF _ bs _         -> mconcat $ map (f . fst) bs
  _                       -> mempty

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

mapNameExp :: (Name -> Name) -> Exp -> Exp
mapNameExp f = \case
  Program e c s d -> Program e c [sd {sName = f $ sName sd} | sd <- s] d
  Def n args e    -> Def (f n) [(f a, t) | (a, t) <- args] e
  exp             -> mapLocalNameExp f exp

mapLocalNameExp :: (Name -> Name) -> Exp -> Exp
mapLocalNameExp f = \case
  -- def
  Def n args e            -> Def n [(f n, t) | (n, t) <- args] e -- NOTE: do not touch top level function name
  LetRec bs e             -> LetRec [(f n, t, a) | (n, t, a) <- bs] e
  Let bs e                -> Let  [(f n, t, a) | (n, t, a) <- bs] e
  LetS bs e               -> LetS [(f n, t, a) | (n, t, a) <- bs] e
  Alt a (NodePat n bs) e  -> Alt (f a) (NodePat n (map f bs)) e
  Closure vs bs e         -> Closure (map f vs) [(f n, t) | (n, t) <- bs] e
  -- use
  App n l                 -> App (f n) (map f l)
  Case n a                -> Case (f n) a
  Con n l                 -> Con (f n) (map f l)
  Var n                   -> Var (f n)
  -- other
  exp                     -> exp

isAtom :: Exp -> Bool
isAtom = \case
  Var{} -> True
  Lit{} -> True
  _     -> False

-- merges let seqences of the same kind
smashLet :: Exp -> Exp
smashLet = cata folder where
  folder = \case
    LetF l1 (Let l2 e)    -> Let (l1 ++ l2) e
    LetSF l1 (LetS l2 e)  -> LetS (l1 ++ l2) e
    e                     -> embed e

mapNameTy :: (Name -> Name) -> Ty -> Ty
mapNameTy f = \case
  TyCon n c l   -> TyCon (f n) c l
  TyVar n       -> TyVar (f n)
  TySimple n t  -> TySimple (f n) t
  TyFun n t l   -> TyFun (f n) t l

foldNameTyF :: (Monoid m) => (Name -> m) -> TyF a -> m
foldNameTyF f = \case
  TyConF n _ _  -> f n
  TyVarF n      -> f n
  TySimpleF n _ -> f n
  TyFunF n _ _  -> f n
