{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGenBoxed (codegenGrin) where

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
  | P G.LPat G.Exp

data Repr
  = Repr
  { ptrName :: Maybe Name
  , valName :: Maybe Name
  }

instance Semigroup Repr where
  Repr a1 b1 <> Repr a2 b2 = Repr (a1 <> a2) (b1 <> b2)

instance Monoid Repr where
  mempty = Repr Nothing Nothing

data Env
  = Env
  { _arityMap   :: Map Name Int
  , _commands   :: [Cmd]
  , _varMap     :: Map Name Repr
  }

type CG = StateT Env NameM

addRepr :: Name -> Repr -> CG ()
addRepr n r = modify $ \env@Env{..} -> env {_varMap = Map.insertWith mappend n r _varMap}

valN, ptrN :: Name -> Repr
valN n = Repr Nothing (Just n)
ptrN n = Repr (Just n) Nothing

getPtr :: Name -> CG (Maybe Name)
getPtr n = do
  m <- gets _varMap
  pure $ do
    Repr p v <- Map.lookup n m
    p

getVal :: Name -> CG (Maybe Name)
getVal n = do
  m <- gets _varMap
  pure $ do
    Repr p v <- Map.lookup n m
    v

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

genCPat :: Pat -> G.CPat
genCPat = \case
  NodePat name args -> G.NodePat (G.Tag G.C name) args
  LitPat  lit       -> G.LitPat (genLit lit)
  DefaultPat        -> G.DefaultPat

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
clearState = modify $ \env -> env {_commands = [], _varMap = mempty}

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
      go [P p e] = G.EBind e p (G.SReturn p)
      go [B n e] = G.EBind e (G.Var n) (G.SReturn $ G.Var n)
      go ((G e) : xs) = G.EBind e G.Unit $ go xs
      go ((P p e) : xs) = G.EBind e p $ go xs
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
  Var _ "GHC.Prim.void#" -> pure . G.Lit . G.LBool $ False

  Var isPtr name -> getPtr name >>= \case
    Just n -> pure $ G.Var n
    Nothing -> do
      ptrName <- tmpName "ptr_" name
      addRepr name $ ptrN ptrName
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

  Var isPtr name -> arityM name >>= \case
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
  Var isPtr name
    | Nothing <- arity _arityMap name -> do
      getPtr name >>= \case
        Nothing -> pure $ G.SReturn $ G.Var name
        Just n  -> pure $ G.SApp "eval" [G.Var n]

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

  Var isPtr name
    | Just ar <- arity _arityMap name -> do
      pure . G.SReturn $ mkThunk ar name []

    | otherwise -> do
      getPtr name >>= \case
        Nothing -> pure $ G.SReturn $ G.Var name
        Just n  -> pure $ G.SApp "eval" [G.Var n]

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
    forM_ binds $ \(name, rhs) -> do
      genStrictExp rhs >>= emit . B name
      addRepr name $ valN name
    genExp exp

  Let binds exp -> do
    forM_ binds $ \(name, rhs) -> do
      genLazyExp rhs >>= emit . B name
      addRepr name $ ptrN name
    genExp exp

  Case (Var isPtr name) alts -> do
    let litTags = [boxedLitTag l | Alt (LitPat l) _ <- alts]

    scrutName <- if null litTags then pure name else if any (head litTags /=) litTags
      then error $ "invalid case expression: " ++ show lambdaExp
      else do
        let litTag = head litTags
        litName <- uniq litTag
        emit $ P (mkCon litTag [G.Var litName]) (G.SReturn $ G.Var name)
        pure litName

    altExps <- forM alts $ \(Alt pat rhs) -> fmap (G.Alt (genCPat pat)) $ withBlock $ do
      case pat of
        NodePat _ args -> forM_ args $ \n -> addRepr n $ ptrN n
        _ -> pure ()
      genExp rhs >>= emit . G

    resultName <- tmpName "result_" name
    emit . B resultName . G.SBlock . G.ECase (G.Var scrutName) $ altExps
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
    forM_ args $ \n -> addRepr n $ ptrN n
    result <- genExp exp
    emit $ G result
    G.Def name args <$> cmdToExp

  evalFun <- lift $ genEval mempty "eval" grinDefs
  applyDef <- lift $ genApply mempty "apply" grinDefs

  let G.Program ghcPrimExts ghcPrimDefs = ghcPrimOps

  pure . staticSingleAssignment $ G.Program (ghcPrimExts ++ exts) $ ghcPrimDefs ++ evalFun : applyDef : grinDefs

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (evalStateT (genProgram exp) emptyEnv) (mkNameEnv exp) where
  emptyEnv = Env
    { _arityMap   = buildArityMap exp
    , _commands   = []
    , _varMap     = mempty
    }

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program _ defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
