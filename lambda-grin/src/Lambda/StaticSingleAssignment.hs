{-# LANGUAGE LambdaCase, TupleSections #-}
module Lambda.StaticSingleAssignment where

import Text.Printf
import Control.Monad.State
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable

import Transformations.Names hiding (mkNameEnv)
import Transformations.Util hiding (foldNameDefExpF)
import Lambda.Syntax

type Env = Map Name Name

mkNameEnv :: Exp -> NameEnv
mkNameEnv exp = NameEnv mempty (cata folder exp) where
  folder e = foldNameDefExpF Set.singleton e `mappend` Data.Foldable.fold e

singleStaticAssignment :: Exp -> Exp
singleStaticAssignment e = evalState (anaM build (mempty, e)) (mkNameEnv e) where

  mkName :: (Env, [(Name, (Env, Exp))]) -> (Name, Exp) -> NameM (Env, [(Name, (Env, Exp))])
  mkName (env', l) (n, b) = do
    n' <- deriveNewName n
    pure (Map.insert n n' env', l ++ [(n', (env', b))])

  build :: (Env, Exp) -> NameM (ExpF (Env, Exp))
  build (env, e) = case e of
    -- name shadowing in the bind sequence
    Let bs e  -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetF bs' (newEnv, e)

    -- name shadowing in the bind sequence
    LetS bs e  -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetSF bs' (newEnv, e)

    -- no name shadowing
    _ -> do
      newEnv <- foldM (\m n -> Map.insert n <$> deriveNewName n <*> pure m) env $ (foldNameDefExp (:[]) e :: [Name])
      pure $ (newEnv,) <$> (project $ mapNameExp (subst newEnv) e)

foldNameDefExp :: (Monoid m) => (Name -> m) -> Exp -> m
foldNameDefExp f = foldNameDefExpF f . project

foldNameDefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameDefExpF f = \case
  DefF _ args _         -> mconcat $ map f args
  LetF bs _             -> mconcat $ map (f . fst) bs
  LetRecF bs _          -> mconcat $ map (f . fst) bs
  LetSF bs _            -> mconcat $ map (f . fst) bs
  AltF (NodePat _ bs) _ -> mconcat $ map f bs
  LamF bs _             -> mconcat $ map f bs
  _                     -> mempty

mapNameExp :: (Name -> Name) -> Exp -> Exp
mapNameExp f = \case
  -- def
  Def n args e          -> Def n (map f args) e -- NOTE: do not touch top level function name
  LetRec bs e           -> LetRec [(f n, a) | (n,a) <- bs] e
  Let bs e              -> LetS [(f n, a) | (n,a) <- bs] e
  LetS bs e             -> LetS [(f n, a) | (n,a) <- bs] e
  Alt (NodePat n bs) e  -> Alt (NodePat n (map f bs)) e
  Lam bs e              -> Lam (map f bs) e
  -- use
  App n l               -> App (f n) l
  Var n                 -> Var (f n)
  -- other
  exp                   -> exp
