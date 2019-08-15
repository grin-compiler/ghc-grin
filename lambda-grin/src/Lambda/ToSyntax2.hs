{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.ToSyntax2 where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Data.List (groupBy)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import qualified Lambda.Syntax as L
import Lambda.Syntax2

import Transformations.Util
import Transformations.Names hiding (mkNameEnv)
import Lambda.Util

type SM = WriterT [(Name, L.Exp)] NameM

simplifyArgs :: L.Exp -> SM L.Exp
simplifyArgs = \case
  L.Con n l   -> L.Con n <$> mapM simplifyVar l
  L.App n l   -> L.App n <$> mapM simplifyVar l
  L.Case e a  -> L.Case <$> simplifyVar e <*> pure a
  exp -> pure exp

simplifyVar :: L.Exp -> SM L.Exp
simplifyVar exp
  | isSimpleExp exp = L.Var False <$> simplify exp
  | otherwise       = pure exp

simplify :: L.Exp -> SM Name
simplify = \case
  -- Atom
  L.Var _ n -> pure n
  exp -> do
    e <- simplifyArgs exp
    name <- lift $ deriveNewName $ "val"
    tell [(name, e)]
    pure name

isSimpleExp :: L.Exp -> Bool
isSimpleExp = \case
  L.App{}     -> True
  L.Case{}    -> True
  L.Con{}     -> True
  L.Lit{}     -> True
  L.Closure{} -> True
  _ -> False

data Ctx = Strict | Lazy

simplifyExp :: Ctx -> L.Exp -> NameM L.Exp -- returns a LetS
simplifyExp ctx exp
  | isSimpleExp exp = do
      (name, binds) <- runWriterT $ simplify exp
      pure $ case ctx of
        Strict  -> L.LetS binds $ L.Var False name
        Lazy    -> L.Let  binds $ L.Var False name

  | otherwise = pure exp

isVar :: L.Exp -> Bool
isVar = \case
  L.Var{} -> True
  _ -> False

toSyntax2M :: L.Program -> NameM Program
toSyntax2M = hyloM folder builder where

  builder :: L.Exp -> NameM (ExpF L.Exp)
  builder = \case
    L.Program e d -> pure $ ProgramF e d

    -- bind sequences
    L.Def n a e -> DefF n a <$> simplifyExp Strict e
    L.Closure v a e -> ClosureF v a <$> simplifyExp Strict e
    L.Alt p e -> AltF <$> deriveNewName "alt" <*> pure p <*> simplifyExp Strict e

    L.Let l e -> do
      l2 <- forM l $ \(n,se) -> do
        (se2, se2Binds) <- runWriterT $ simplifyArgs se
        pure $ se2Binds ++ [(n,se2)]
      LetF (concat l2) <$> simplifyExp Strict e

    L.LetS l e -> do
      l2 <- forM l $ \(n,se) -> do
        (se2, se2Binds) <- runWriterT $ simplifyArgs se
        pure $ se2Binds ++ [(n,se2)]
      LetSF (concat l2) <$> simplifyExp Strict e

    L.LetRec l e -> do
      l2 <- forM l $ \(n,se) -> do
        (se2, se2Binds) <- runWriterT $ simplifyArgs se
        pure $ se2Binds ++ [(n,se2)]
      LetRecF (concat l2) <$> simplifyExp Strict e

    -- (already simplified simple exps)
    L.App n l | all isVar l -> pure $ AppF n [a | L.Var _ a <- l]
    L.Con n l | all isVar l -> pure $ ConF n [a | L.Var _ a <- l]
    L.Case (L.Var _ n) l -> pure $ CaseF n l
    L.Lit l -> pure $ LitF l
    L.Var _ n -> pure $ VarF n

    -- impossible / internal error
    exp -> error $ "simple exp was not simplified: " ++ show exp

  folder = pure . smash . project . flat

-- flattens nested lets
data LetKind = S | L | R deriving Eq

flat :: ExpF Exp -> Exp
flat = \case
  LetF l e    -> mkBind (groupBy letEq $ concatMap (mark L) l) e
  LetSF l e   -> mkBind (groupBy letEq $ concatMap (mark S) l) e
  LetRecF l e -> mkBind (groupBy letEq $ concatMap (mark R) l) e
  e -> embed e

letEq a b = fst a == fst b

mkBind [] e = e
mkBind (x@((k,_):_) : xs) exp = case k of
  L -> Let    (map snd x) $ mkBind xs exp
  S -> LetS   (map snd x) $ mkBind xs exp
  R -> LetRec (map snd x) $ mkBind xs exp

mark k@R (n, exp) = case exp of
  Let l e     -> map (R,) l ++ mark k (n, e)
  LetS l e    -> error $ "letS is not allowed in letrec RHS: " ++ show exp
  LetRec l e  -> map (R,) l ++ mark k (n, e)
  _ -> [(k, (n, exp))]
mark k (n, exp) = case exp of
  Let l e     -> map (L,) l ++ mark k (n, e)
  LetS l e    -> map (S,) l ++ mark k (n, e)
  LetRec l e  -> map (R,) l ++ mark k (n, e)
  _ -> [(k, (n, exp))]

-- merges let seqences of the same kind
smash = \case
  LetF l1 (Let l2 e)    -> Let (l1 ++ l2) e
  LetSF l1 (LetS l2 e)  -> LetS (l1 ++ l2) e
  e                     -> embed e

toSyntax2 :: L.Program -> Program
toSyntax2 prg = evalState (toSyntax2M prg) (mkNameEnv prg)
