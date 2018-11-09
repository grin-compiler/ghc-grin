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

  add :: Env -> (Name, Name) -> Env
  add env (k,v) = Map.insert k v env

  addMany :: Env -> [(Name, Name)] -> Env
  addMany = foldl add

  build :: (Env, Exp) -> NameM (ExpF (Env, Exp))
  build (env, e) = case e of

    -- name shadowing in the bind sequence

    Let bs e -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetF bs' (newEnv, e)

    LetS bs e -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetSF bs' (newEnv, e)

    LetRec bs e -> do
      let ns = map fst bs
      newNs <- mapM deriveNewName ns
      let newEnv = addMany env $ zip ns newNs
      pure $ LetRecF [(n, (newEnv, b)) | (n, (_, b)) <- zip newNs bs] (newEnv, e)

    Lam ns e -> do
      newNs <- mapM deriveNewName ns
      pure $ LamF newNs (addMany env $ zip ns newNs, e)

    Def n ns e -> do
      newNs <- mapM deriveNewName ns
      pure $ DefF n newNs (addMany env $ zip ns newNs, e)

    Alt (NodePat n ns) e -> do
      newNs <- mapM deriveNewName ns
      pure $ AltF (NodePat n newNs) (addMany env $ zip ns newNs, e)

    -- no name shadowing

    _ -> do
      newEnv <- foldM (\m n -> Map.insert n <$> deriveNewName n <*> pure m) env $ (foldLocalNameDefExp (:[]) e :: [Name])
      pure $ (newEnv,) <$> (project $ mapLocalNameExp (subst newEnv) e)
