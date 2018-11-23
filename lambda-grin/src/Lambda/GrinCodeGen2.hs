{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGen2 (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import qualified Data.Text.Short as TS
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
import Lambda.Util
import Lambda.GhcPrimOps
import qualified Grin.Grin as G
import Transformations.Optimising.DeadProcedureElimination
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
  , _pointerMap :: Map Name (Maybe Name)  -- pointer/primType -> value
  , _valueMap   :: Map Name (Maybe Name)  -- value -> pointer/primType
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
-- TODO
{-
data Lit
  | LChar   Char
  | LString ByteString
  -- special
  | LError  String  -- marks an error
  | LDummy  String  -- should be ignored
-}
genCPat :: Pat -> G.CPat
genCPat = \case
  NodePat name args -> G.NodePat (G.Tag G.C name) args
  LitPat  lit       -> G.LitPat (genLit lit)
  DefaultPat        -> G.DefaultPat

mkCon :: Name -> [G.Val] -> G.Val
mkCon name args = G.ConstTagNode (G.Tag G.C name) args

mkThunk :: Int -> Name -> [G.Val] -> G.Val
mkThunk arity name args = G.ConstTagNode (G.Tag (if missing == 0 then G.F else G.P missing) name) args where
  missing = arity - length args

emit :: Cmd -> CG ()
emit cmd = modify $ \env@Env{..} -> env {_commands = cmd : _commands}

clearState :: CG ()
clearState = modify $ \env -> env {_commands = [], _pointerMap = mempty, _valueMap = mempty}

clearCmds :: CG ()
clearCmds = modify $ \env -> env {_commands = []}

addValueNames :: [Name] -> CG ()
addValueNames names = modify $ \env@Env{..} -> env {_valueMap = foldl (\m k -> Map.insertWith mappend k Nothing m) _valueMap names}

addPointerNames :: [Name] -> CG ()
addPointerNames names = modify $ \env@Env{..} -> env {_pointerMap = foldl (\m k -> Map.insertWith mappend k Nothing m) _pointerMap names}

addPointerName :: Name -> Maybe Name-> CG ()
addPointerName name valueName = modify $ \env@Env{..} -> env {_pointerMap = Map.insertWith mappend name valueName _pointerMap}

addValueName :: Name -> Maybe Name-> CG ()
addValueName name pointerName = modify $ \env@Env{..} -> env {_valueMap = Map.insertWith mappend name pointerName _valueMap}

addPtrAndValueName :: Name -> Name -> CG ()
addPtrAndValueName ptrName valueName = do
  addPointerName ptrName (Just valueName)
  addValueName valueName (Just ptrName)

getPointerName :: Name -> CG (Maybe Name)
getPointerName name = do
  Env{..} <- get
  pure $ if Map.member name _pointerMap
    then Just name
    else join $ Map.lookup name _valueMap

isValueName :: Name -> CG Bool
isValueName name = do
  Env{..} <- get
  pure $ Map.member name _valueMap

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
  Lit lit -> pure . G.Lit $ genLit lit

  Con name args -> do
    vals <- genVals args
    ptrName <- tmpName "ptr_" name
    emit . B ptrName $ G.SStore $ mkCon name vals
    pure $ G.Var ptrName

  -- FIXME: handle GHC prim types properly
  Var "GHC.Prim.void#" -> pure . G.Lit . G.LBool $ False

  Var name -> arityM name >>= \case
    Nothing -> do
      getPointerName name >>= \case
        Just ptrName -> pure $ G.Var ptrName
        Nothing -> do
          ptrName <- tmpName "ptr_" name
          addPtrAndValueName ptrName name
          emit . B ptrName $ G.SStore $ G.Var name
          pure $ G.Var ptrName

    Just ar -> do
      ptrName <- tmpName "ptr_" name
      emit . B ptrName $ G.SStore $ mkThunk ar name []
      pure $ G.Var ptrName

  x -> error $ printf "unsupported atom: %s" (show x)

genPrimOpVal :: Atom -> CG G.Val
genPrimOpVal = \case
  Lit lit -> pure . G.Lit $ genLit lit

  -- FIXME: handle GHC prim types properly
  Var "GHC.Prim.void#" -> pure . G.Lit . G.LBool $ False

  Var name -> pure $ G.Var name

  x -> error $ printf "unsupported atom: %s" (show x)

genVals :: [Exp] -> CG [G.Val]
genVals = mapM genVal

genLazyExp :: Exp -> CG G.SimpleExp
genLazyExp lambdaExp = get >>= \Env{..} -> case lambdaExp of

  Con name args -> do
    G.SStore . mkCon name <$> genVals args

  Var name -> arityM name >>= \case
    --Nothing -> pure $ G.Var name
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

  App name args | isGhcPrim name -> G.SApp name <$> mapM genPrimOpVal args

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
          addValueName fName Nothing
          let applyArg n arg = do
                newName <- tmpName "result_" name
                emit (B newName $ G.SApp "apply" [G.Var n, arg])
                case arg of
                  G.Var argName -> addPtrAndValueName argName newName
                  _ -> pure ()
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

    | otherwise -> do
      isValueName name >>= \case
        False -> pure $ G.SApp "eval" [G.Var name]
        True  -> pure . G.SReturn $ G.Var name

  App name args | isGhcPrim name -> G.SApp name <$> mapM genPrimOpVal args

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
          addValueName fName Nothing
          let applyArg n arg = do
                newName <- tmpName "result_" name
                emit (B newName $ G.SApp "apply" [G.Var n, arg])
                case arg of
                  G.Var argName -> addPtrAndValueName argName newName
                  _ -> pure ()
                pure newName
          G.SReturn . G.Var <$> foldM applyArg fName extraArgs
      _ -> do
        vals <- genVals args
        fName <- tmpName "result_" name
        emit $ B fName $ G.SApp "eval" [G.Var name]
        addPtrAndValueName name fName
        let applyArg n arg = do
              newName <- tmpName "result_" name
              emit (B newName $ G.SApp "apply" [G.Var n, arg])
              case arg of
                G.Var argName -> addPtrAndValueName argName newName
                _ -> pure ()
              pure newName
        G.SReturn . G.Var <$> foldM applyArg fName vals

  LetS binds exp -> do
    forM_ binds $ \(name, rhs) -> case rhs of
      App opName _ | isGhcPrim opName && isUnboxedPrim opName -> genExp rhs >>= emit . B name >> addPointerNames [name]
      _ -> genStrictExp rhs >>= emit . B name >> addValueNames [name]
    genExp exp

  Let binds exp -> do
    forM_ binds $ \(name, rhs) -> genLazyExp rhs >>= emit . B name >> addPointerNames [name]
    genExp exp

  Case (Var name) alts -> do
    altExps <- forM alts $ \(Alt pat rhs) -> fmap (G.Alt (genCPat pat)) $ withBlock $ do
      case pat of
        NodePat _ args -> addPointerNames args
        _ -> pure ()
      result <- genExp rhs
      emit $ G result

    resultName <- tmpName "result_" name
    emit . B resultName . G.SBlock $ G.ECase (G.Var name) altExps
    pure . G.SReturn $ G.Var resultName

  exp -> error $ "genExp: " ++ show exp

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
    addPointerNames args
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
    , _pointerMap = mempty
    , _valueMap   = mempty
    , _commands   = []
    }

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
