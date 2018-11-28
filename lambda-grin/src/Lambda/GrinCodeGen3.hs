{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGen3 (codegenGrin) where

{-
  Experimental Lambda to GRIN coegen
  It translates Lambda literals to boxed GRIN values.
-}

import Text.Printf
import Control.Monad
import Control.Monad.State

import qualified Data.Text.Short as TS
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
import Lambda.Util
import Lambda.GhcPrimOpsBoxed
import qualified Grin.Grin as G
import Transformations.StaticSingleAssignment
import Transformations.GenerateEval
import Transformations.Util
import Transformations.Names hiding (mkNameEnv)

data Cmd
  = G G.Exp
  | B Name G.Exp

data Env
  = Env
  { _arityMap   :: Map Name Int
  , _commands   :: [Cmd]
  }

type CG = StateT Env NameM

uniq :: Name -> CG Name
uniq = lift . deriveNewName

arity :: Map Name Int -> Name -> Maybe Int
arity = flip Map.lookup

arityM :: Name -> CG (Maybe Int)
arityM name = arity <$> gets _arityMap <*> pure name

genLit :: Lit -> G.Lit
genLit = \case
  LInt64  v -> G.LInt64 v
  LWord64 v -> G.LWord64 v
  LFloat  v -> G.LFloat $ fromRational v
  LBool   v -> G.LBool v
  _ -> G.LWord64 999 -- TODO

boxedLitTag :: Lit -> Name
boxedLitTag = \case
  LInt64{}  -> "int64"
  LWord64{} -> "word64"
  LFloat{}  -> "float"
  LBool{}   -> "bool"
  _         -> "error"

genBoxedLit :: Lit -> G.Val
genBoxedLit l = mkCon (boxedLitTag l) [G.Lit $ genLit l]

-- TODO
{-
data Lit
  | LChar   Char
  | LString ByteString
  -- special
  | LError  String  -- marks an error
  | LDummy  String  -- should be ignored
-}

mkCon :: Name -> [G.Val] -> G.Val
mkCon name args = G.ConstTagNode (G.Tag G.C name) args

mkThunk :: Int -> Name -> [G.Val] -> G.Val
mkThunk arity name args = G.ConstTagNode (G.Tag (if missing == 0 then G.F else G.P missing) name) args where
  missing = arity - length args

emit :: Cmd -> CG ()
emit cmd = modify $ \env@Env{..} -> env {_commands = cmd : _commands}

clearState :: CG ()
clearState = modify $ \env -> env {_commands = []}

clearCmds :: CG ()
clearCmds = modify $ \env -> env {_commands = []}

withBlock :: CG () -> CG G.Exp
withBlock m = do
  savedCmds <- gets _commands
  clearCmds
  m
  exp <- cmdToExp
  modify $ \env@Env{..} -> env {_commands = savedCmds}
  pure exp

cmdToExp :: CG G.Exp
cmdToExp = do
  let go [] = G.SReturn G.Unit
      go [G e] = e
      go [B n e] = G.EBind e (G.Var n) (G.SReturn $ G.Var n)
      go ((G e) : xs) = G.EBind e G.Unit $ go xs
      go ((B n e) : xs) = G.EBind e (G.Var n) $ go xs
  cmds <- gets _commands
  pure $ go $ reverse cmds

tmpName :: Name -> Name -> CG Name
tmpName prefix name = uniq (prefix <> name)

genVal :: Atom -> CG G.Val
genVal = \case
  Lit lit -> do
    let val = genBoxedLit lit
    ptrName <- tmpName "ptr_" "lit"
    emit . B ptrName $ G.SStore val
    pure $ G.Var ptrName

  Con name args -> do
    vals <- genVals args
    ptrName <- tmpName "ptr_" name
    emit . B ptrName $ G.SStore $ mkCon name vals
    pure $ G.Var ptrName

  -- FIXME: handle GHC prim types properly
  Var "GHC.Prim.void#" -> pure . G.Lit . G.LBool $ False

  Var name -> do
    ptrName <- tmpName "ptr_" name
    arityM name >>= \case
      Nothing -> emit . B ptrName $ G.SStore $ G.Var name
      Just ar -> emit . B ptrName $ G.SStore $ mkThunk ar name []
    pure $ G.Var ptrName

  x -> error $ printf "unsupported atom: %s" (show x)

genVals :: [Exp] -> CG [G.Val]
genVals = mapM genVal

genLazyExp :: Exp -> CG G.SimpleExp
genLazyExp lambdaExp = get >>= \Env{..} -> case lambdaExp of

  Con name args -> do
    G.SStore . mkCon name <$> genVals args

  Var name -> arityM name >>= \case
    Just ar -> pure $ G.SStore $ mkThunk ar name []

  App name args
    | argCount <- length args
    -> case arity _arityMap name of
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ -> G.SStore . mkThunk ar name <$> genVals args

  exp -> error $ "genLazyExp: " ++ show exp

isGhcPrim :: Name -> Bool
isGhcPrim = TS.isPrefixOf "_ghc_"

genStrictExp :: Exp -> CG G.SimpleExp
genStrictExp lambdaExp = get >>= \Env{..} -> case lambdaExp of
  Var name
    | Nothing <- arity _arityMap name -> do
      pure $ G.SApp "eval" [G.Var name]

  App name args | isGhcPrim name -> G.SApp name <$> mapM genVal args

  App name args
    | argCount <- length args
    -> case arity _arityMap name of
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ -> G.SApp name <$> genVals args
        GT -> do
          vals <- genVals args
          let (funArgs, extraArgs) = splitAt ar vals
          fName <- tmpName "result_" name
          emit $ B fName $ G.SApp name funArgs
          let applyArg n arg = do
                newName <- tmpName "result_" name
                emit (B newName $ G.SApp "apply" [G.Var n, arg])
                pure newName
          G.SReturn . G.Var <$> foldM applyArg fName extraArgs

  exp -> error $ "genStrictExp: " ++ show exp

genExp :: Exp -> CG G.SimpleExp
genExp lambdaExp = get >>= \Env{..} -> case lambdaExp of
  Con name args -> do
    vals <- genVals args
    pure . G.SReturn $ mkCon name vals

  Var name
    | Just ar <- arity _arityMap name -> do
      pure . G.SReturn $ mkThunk ar name []

    | otherwise -> pure $ G.SApp "eval" [G.Var name]

  App name args | isGhcPrim name -> G.SApp name <$> mapM genVal args

  App name args
    | argCount <- length args
    -> case arity _arityMap name of
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ -> G.SApp name <$> genVals args
        GT -> do
          vals <- genVals args
          let (funArgs, extraArgs) = splitAt ar vals
          fName <- tmpName "result_" name
          emit $ B fName $ G.SApp name funArgs
          let applyArg n arg = do
                newName <- tmpName "result_" name
                emit (B newName $ G.SApp "apply" [G.Var n, arg])
                pure newName
          G.SReturn . G.Var <$> foldM applyArg fName extraArgs
      _ -> do
        vals <- genVals args
        fName <- tmpName "result_" name
        emit $ B fName $ G.SApp "eval" [G.Var name]
        let applyArg n arg = do
              newName <- tmpName "result_" name
              emit (B newName $ G.SApp "apply" [G.Var n, arg])
              pure newName
        G.SReturn . G.Var <$> foldM applyArg fName vals

  LetS binds exp -> do
    forM_ binds $ \(name, rhs) -> genStrictExp rhs >>= emit . B name
    genExp exp

  Let binds exp -> do
    forM_ binds $ \(name, rhs) -> genLazyExp rhs >>= emit . B name
    genExp exp

  Case (Var name) alts -> do
    let genAlt (Alt pat rhs) = fmap (G.Alt (genCPat pat)) $ withBlock $ genExp rhs >>= emit . G

        (litAlts, otherAlts) = partition isLitAlt alts
        litPatKinds = Map.toList $ Map.unionsWith (++) [Map.singleton (boxedLitTag l) [alt] | alt@(Alt (LitPat l) _) <- litAlts]

    otherExps <- mapM genAlt otherAlts
    litExps <- forM litPatKinds $ \(litTag, ls) -> do
      litName <- uniq litTag
      litExps <- mapM genAlt ls
      pure . G.Alt (G.NodePat (G.Tag G.C litTag) [litName]) . G.SBlock $ G.ECase (G.Var litName) litExps


    resultName <- tmpName "result_" name
    emit . B resultName . G.SBlock . G.ECase (G.Var name) $ otherExps ++ litExps
    pure . G.SReturn $ G.Var resultName

  exp -> error $ "genExp: " ++ show exp

isLitAlt (Alt p _) = isLitPat p

isLitPat :: Pat -> Bool
isLitPat = \case
  LitPat {} -> True
  _         -> False

genCPat :: Pat -> G.CPat
genCPat = \case
  NodePat name args -> G.NodePat (G.Tag G.C name) args
  LitPat  lit       -> G.LitPat (genLit lit)
  DefaultPat        -> G.DefaultPat

remapNames :: Program -> [Name] -> CG Program
remapNames prog names = do
  env <- Map.fromList <$> sequence [(n,) <$> uniq n | n <- names]
  pure $ cata (\exp -> mapNameExp (subst env) $ embed exp) prog

genProgram :: Program -> CG G.Program
genProgram prog' = do
  prog <- remapNames prog' ["apply", "eval", "grinMain"]
  let Program defs = prog
  grinDefs <- forM defs $ \(Def name args exp) -> do
    clearState
    result <- genExp exp
    emit $ G result
    G.Def name args <$> cmdToExp

  evalFun <- lift $ genEval mempty "eval" grinDefs
  applyDef <- lift $ genApply mempty "apply" grinDefs

  let G.Program ghcPrimDefs = ghcPrimOps

  pure . staticSingleAssignment $ G.Program $ ghcPrimDefs ++ evalFun : applyDef : grinDefs

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (evalStateT (genProgram exp) emptyEnv) (mkNameEnv exp) where
  emptyEnv = Env
    { _arityMap   = buildArityMap exp
    , _commands   = []
    }

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
