{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.ToSyntax2 where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

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
  L.Var p n -> pure n
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
      (e2, e2Binds) <- runWriterT $ simplifyVar e
      pure $ LetF (concat l2 ++ e2Binds) e2

    L.LetS l e -> do
      l2 <- forM l $ \(n,se) -> do
        (se2, se2Binds) <- runWriterT $ simplifyArgs se
        pure $ se2Binds ++ [(n,se2)]
      (e2, e2Binds) <- runWriterT $ simplifyVar e
      pure $ LetSF (concat l2 ++ e2Binds) e2

    L.LetRec l e -> do
      l2 <- forM l $ \(n,se) -> do
        (se2, se2Binds) <- runWriterT $ simplifyArgs se
        pure $ se2Binds ++ [(n,se2)]
      (e2, e2Binds) <- runWriterT $ simplifyVar e
      pure $ LetRecF (concat l2 ++ e2Binds) e2

    -- (already simplified simple exps)
    L.App n l | all isVar l -> pure $ AppF n [a | L.Var _ a <- l]
    L.Con n l | all isVar l -> pure $ ConF n [a | L.Var _ a <- l]
    L.Case (L.Var _ n) l -> pure $ CaseF n l
    L.Lit l -> pure $ LitF l
    L.Var p n -> pure $ VarF p n

    -- impossible / internal error
    exp -> error $ "simple exp was not simplified: " ++ show exp

  folder = \case
    LetF l1 (Let l2 e)        -> pure $ Let (l1 ++ l2) e
    LetSF l1 (LetS l2 e)      -> pure $ LetS (l1 ++ l2) e
    e                         -> pure $ embed e

test :: Program
test = toSyntax2 (L.Program [] [L.Def "fun" [] $ L.Lit $ L.LInt64 0])

toSyntax2 :: L.Program -> Program
toSyntax2 prg = evalState (toSyntax2M prg) (mkNameEnv prg)
