{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Lambda.StaticSingleAssignment where

import Control.Monad.State
import Data.Functor.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Foldable

import Transformations.Names hiding (mkNameEnv)
import Transformations.Util hiding (foldNameDefExpF)
import Lambda.Syntax
import Lambda.Util

type Env = Map Name Name

ssaExternal :: External -> NameM External
ssaExternal ext@External{..} = do
  let names = Set.toList $ Set.unions [cata (foldNameTyF Set.singleton) t | t <- eRetType : eArgsType]
  env <- forM names $ \n -> do
    newN <- deriveNewName n
    pure (n, newN)
  let substFun :: Ty -> Ty
      substFun t = ana (project . mapNameTy (subst $ Map.fromList env)) t
  pure ext
    { eRetType  = substFun eRetType
    , eArgsType = map substFun eArgsType
    }

singleStaticAssignment :: Exp -> Exp
singleStaticAssignment e = evalState (anaM build (mempty, e)) (mkNameEnv e) where

  mkName :: (Env, [(Name, RepType, (Env, Exp))]) -> (Name, RepType, Exp) -> NameM (Env, [(Name, RepType, (Env, Exp))])
  mkName (env', l) (n, t, b) = do
    n' <- deriveNewName n
    pure (Map.insert n n' env', l ++ [(n', t, (env', b))])

  add :: Env -> (Name, Name) -> Env
  add env (k,v) = Map.insert k v env

  addMany :: Env -> [(Name, Name)] -> Env
  addMany = foldl add

  build :: (Env, Exp) -> NameM (ExpF (Env, Exp))
  build (env, e) = case e of

    Program{..} -> do
      newExts <-  mapM ssaExternal pExternals
      pure $ ProgramF
        { pExternalsF     = newExts
        , pConstructorsF  = pConstructors
        , pPublicNamesF   = pPublicNames
        , pStaticDataF    = pStaticData
        , pDefinitionsF   = map (env,) pDefinitions
        }

    -- name shadowing in the bind sequence

    Let bs e -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetF bs' (newEnv, e)

    LetS bs e -> do
      (newEnv, bs') <- foldM mkName (env,[]) bs
      pure $ LetSF bs' (newEnv, e)

    LetRec bs e -> do
      let ns = map fst3 bs
      newNs <- mapM deriveNewName ns
      let newEnv = addMany env $ zip ns newNs
      pure $ LetRecF [(n, t, (newEnv, b)) | (n, (_, t, b)) <- zip newNs bs] (newEnv, e)

    Closure vs args e -> do
      let (argNames, argTypes) = unzip args
      newNs <- mapM deriveNewName argNames
      pure $ ClosureF (map (subst env) vs) (zip newNs argTypes) (addMany env $ zip argNames newNs, e)

    Def n args e -> do
      let (argNames, argTypes) = unzip args
      newNs <- mapM deriveNewName argNames
      pure $ DefF n (zip newNs argTypes) (addMany env $ zip argNames newNs, e)

    Alt a (NodePat n ns) e -> do
      newA <- deriveNewName a
      newNs <- mapM deriveNewName ns
      pure $ AltF newA (NodePat n newNs) (addMany env $ zip ns newNs, e)

    -- no name shadowing

    _ -> do
      newEnv <- foldM (\m n -> Map.insert n <$> deriveNewName n <*> pure m) env $ (foldLocalNameDefExp (:[]) e :: [Name])
      pure $ (newEnv,) <$> (project $ mapLocalNameExp (subst newEnv) e)
