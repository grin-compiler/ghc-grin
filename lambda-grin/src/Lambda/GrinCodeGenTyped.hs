{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGenTyped (codegenGrin) where

{-
  Experimental Lambda to GRIN codegen with unboxed literal and primitve type support.
-}

import Text.Printf
import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
import Lambda.Util
import Lambda.GhcPrimOps
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
  { _arityMap :: Map Name Int
  , _commands :: [Cmd]
  , _valueMap :: Map Name (Maybe Name)
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
  LError  e -> error $ T.unpack e
  LString v -> G.LString . T.pack . BS8.unpack $ v
  LChar   v -> G.LChar v
  LLabelAddr v  -> G.LString $ T.pack . BS8.unpack $ v -- TODO
  LNullAddr     -> G.LString "LNullAddr"
  l         -> error $ "unsupported: " ++ show l
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
clearState = modify $ \env -> env {_commands = [], _valueMap = mempty}

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
      go [G e] = f e
      go [B n1 e, G (G.SReturn (G.Var n2))] | n1 == n2 = f e -- G.EBind e (G.Var n) (G.SReturn $ G.Var n)
      go [B n e] = f e -- G.EBind e (G.Var n) (G.SReturn $ G.Var n)
      go ((G e) : xs) = G.EBind e G.Unit $ go xs
      go ((B n e) : xs) = G.EBind e (G.Var n) $ go xs

      f (G.SBlock e@G.ECase{}) = e
      f e = e
  cmds <- gets _commands
  pure $ go $ reverse cmds

tmpName :: Name -> Name -> CG Name
tmpName prefix name = uniq (prefix <> name)

addValueName :: Name -> Maybe Name -> CG ()
addValueName name pointerName = modify $ \env@Env{..} -> env {_valueMap = Map.insertWith mappend name pointerName _valueMap}

getPtrName :: Name -> CG Name
getPtrName n = do
  gets (Map.lookup n . _valueMap) >>= \case
    Nothing         -> pure n
    Just (Just pn)  -> pure pn
    Just Nothing -> do
      ptrName <- tmpName "ptr_" n
      addValueName n $ Just ptrName
      emit . B ptrName $ G.SStore $ G.Var n
      pure ptrName

genVal :: Atom -> CG G.Val
genVal = \case
  Lit lit -> pure . G.Lit $ genLit lit

  Con name args -> do
    vals <- genVals args
    ptrName <- tmpName "ptr_" name
    emit . B ptrName $ G.SStore $ mkCon name vals
    pure $ G.Var ptrName

  Var isPtr name -> arityM name >>= \case
    Nothing
      | not isPtr -> pure $ G.Var name
      | otherwise -> G.Var <$> getPtrName name

    Just ar -> do
      ptrName <- tmpName "ptr_" name
      emit . B ptrName $ G.SStore $ mkThunk ar name []
      pure $ G.Var ptrName

  x -> error $ printf "unsupported atom: %s" (show x)

genVals :: [Exp] -> CG [G.Val]
genVals = mapM genVal

genLazyExpVal :: Exp -> CG G.Val
genLazyExpVal lambdaExp = get >>= \Env{..} -> case lambdaExp of

  Con name args -> do
    mkCon name <$> genVals args

  Var isPtr name -> arityM name >>= \case
    Nothing
      | not isPtr -> pure $ G.Var name
      | otherwise -> G.Var <$> getPtrName name

    Just ar -> pure $ mkThunk ar name []

  App name args
    | argCount <- length args
    -> case arity _arityMap name of
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ -> mkThunk ar name <$> genVals args
        LT -> mkThunk ar name <$> genVals args
        x  -> error $ "non-matching arity: " ++ show x ++ " for function: " ++ show name

      Nothing -> error $ "unknown function: " ++ show name

  exp -> error $ "genLazyExpVal: " ++ show exp

genLazyExp :: Exp -> CG G.SimpleExp
genLazyExp lambdaExp = do
  val <- genLazyExpVal lambdaExp
  case val of
    G.Var{} -> pure $ G.SReturn val
    _       -> pure $ G.SStore val

isGhcPrim :: Name -> Bool
isGhcPrim = isPrefixOf "_ghc_" . unpackName

genStrictExp :: Exp -> CG G.SimpleExp
genStrictExp lambdaExp = get >>= \Env{..} -> case lambdaExp of
  Var isPtr name
    | Nothing <- arity _arityMap name
    -> if isPtr
        then getPtrName name >>= \n -> pure (G.SApp "eval" [G.Var n])
        else pure . G.SReturn $ G.Var name

    | Just 0 <- arity _arityMap name
    -> pure $ G.SApp name []

  App name args | isGhcPrim name -> G.SApp name <$> genVals args

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
        LT -> error $ "strict function application: " ++ show name

      --Nothing -> error $ "unknown function: " ++ show name
      _ -> do
        vals <- genVals args
        fName <- tmpName "result_" name
        emit $ B fName $ G.SApp "eval" [G.Var name]
        let applyArg n arg = do
              newName <- tmpName "result_" name
              emit (B newName $ G.SApp "apply" [G.Var n, arg])
              pure newName
        G.SReturn . G.Var <$> foldM applyArg fName vals

  Let binds exp -> do
    forM_ binds $ \(name, rhs) -> genLazyExp rhs >>= emit . B name
    genExp exp

  LetS binds exp -> do
    forM_ binds $ \(name, rhs) -> genStrictExp rhs >>= emit . B name >> addValueName name Nothing
    genExp exp

  exp -> error $ "genStrictExp: " ++ show exp

genExp :: Exp -> CG G.SimpleExp
genExp lambdaExp = get >>= \Env{..} -> case lambdaExp of
  Con name args -> do
    vals <- genVals args
    pure . G.SReturn $ mkCon name vals

  Lit lit -> do
    pure . G.SReturn . G.Lit $ genLit lit

  Var isPtr name
    | Just ar <- arity _arityMap name -> do
      pure . G.SReturn $ mkThunk ar name []

    | isPtr     -> getPtrName name >>= \n -> pure $ G.SApp "eval" [G.Var n]
    | not isPtr -> pure . G.SReturn $ G.Var name

  App name args | isGhcPrim name -> G.SApp name <$> genVals args

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
        LT -> G.SReturn . mkThunk ar name <$> genVals args

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
    forM_ binds $ \(name, rhs) -> genStrictExp rhs >>= emit . B name >> addValueName name Nothing
    genExp exp

  Let binds exp -> do
    forM_ binds $ \(name, rhs) -> genLazyExp rhs >>= emit . B name
    genExp exp

  LetRec binds exp -> do
    forM_ binds $ \(name, rhs) -> do
      blackholeName <- tmpName "blackhole_" name
      emit . B name $ G.SStore $ mkCon blackholeName []
    forM_ binds $ \(name, rhs) -> genLazyExpVal rhs >>= emit . G . G.SUpdate name
    genExp exp

  Case (Var isPtr name) alts -> do
    altExps <- forM alts $ \(Alt pat rhs) -> fmap (G.Alt (genCPat pat)) $ withBlock $ do
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
  let Program exts defs = prog
  grinDefs <- forM defs $ \(Def name args exp) -> do
    clearState
    result <- genExp exp
    emit $ G result
    G.Def name args <$> cmdToExp

  evalFun <- lift $ genEval mempty "eval" grinDefs
  applyDef <- lift $ genApply mempty "apply" grinDefs

  let G.Program ghcPrimExts ghcPrimDefs = ghcPrimOps
      exts' = map genExternal exts

  pure . staticSingleAssignment $ G.Program (ghcPrimExts ++ exts') $ ghcPrimDefs ++ evalFun : applyDef : grinDefs

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (evalStateT (genProgram exp) emptyEnv) (mkNameEnv exp) where
  emptyEnv = Env
    { _arityMap = buildArityMap exp
    , _commands = []
    , _valueMap = mempty
    }

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program exts defs) = Map.fromList $ [(name, length args) | Def name args _ <- defs] ++ [(eName, length eArgsType) | External{..} <- exts]
buildArityMap _ = error "invalid expression, program expected"

genExternal :: External -> G.External
genExternal External{..} =
  G.External
  { G.eName       = eName
  , G.eRetType    = genTy eRetType
  , G.eArgsType   = map genTy eArgsType
  , G.eEffectful  = eEffectful
  }

genTy :: Ty -> G.Ty
genTy = \case
  TyCon n tys   -> G.TyCon n (map genTy tys)
  TyVar n       -> G.TyVar n
  TySimple T_Addr -> G.TyCon "T_Addr" []
  TySimple sTy  -> G.TySimple (genSimpleType sTy)

genSimpleType :: SimpleType -> G.SimpleType
genSimpleType = \case
  T_Int64   -> G.T_Int64
  T_Word64  -> G.T_Word64
  T_Float   -> G.T_Float
  T_Bool    -> G.T_Bool
  T_Unit    -> G.T_Unit
  T_String  -> G.T_String
  T_Char    -> G.T_Char
  t         -> error $ "unsupported: " ++ show t
