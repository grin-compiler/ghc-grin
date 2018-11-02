{-# LANGUAGE LambdaCase, TupleSections #-}
module Lambda.StaticSingleAssignment where

import Control.Monad.State
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Foldable

import Transformations.Names hiding (mkNameEnv)
import Transformations.Util hiding (foldNameDefExpF)
import Lambda.Syntax
import Lambda.Util

type Env = Map Name Name

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
      newEnv <- foldM (\m n -> Map.insert n <$> deriveNewName n <*> pure m) env $ (foldLocalNameDefExp (:[]) e :: [Name])
      pure $ (newEnv,) <$> (project $ mapLocalNameExp (subst newEnv) e)
