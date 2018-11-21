{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.GrinCodeGen2 (codegenGrin) where

import Text.Printf
import Control.Monad
import Control.Monad.State

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable

import Lambda.Syntax
import Lambda.Util
--import Lambda.PrimOps
import qualified Grin.Grin as G
import Transformations.Optimising.DeadProcedureElimination
import Transformations.GenerateEval
import Transformations.Util
import Transformations.Names hiding (mkNameEnv)

data Cmd
  = G G.Exp
  | B Name G.Exp

data Env
  = Env
  { _arityMap   :: Map Name Int
  , _whnfMap    :: Map Name Name
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


clearCmds :: CG ()
clearCmds = modify $ \env -> env {_commands = []}

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
    Nothing -> pure $ G.Var name
    Just ar -> do
      ptrName <- tmpName "ptr_" name
      emit . B ptrName $ G.SStore $ mkThunk ar name []
      pure $ G.Var ptrName

  x -> error $ printf "unsupported atom: %s" (show x)

genVals :: [Exp] -> CG [G.Val]
genVals = mapM genVal

genExp :: Exp -> CG ()
genExp lambdaExp = get >>= \Env{..} -> case lambdaExp of
  Con name args -> do
    vals <- genVals args
    emit $ G $ G.SReturn $ mkCon name vals

  Var name | Just ar <- arity _arityMap name -> do
    emit $ G $ G.SReturn $ mkThunk ar name []

  App name args
    | argCount <- length args
    -> case arity _arityMap name of
      -- known function
      Just ar -> case argCount `compare` ar of
        EQ -> do
          vals <- genVals args
          emit $ G $ G.SApp name vals
        GT -> do
          vals <- genVals args
          let (funArgs, extraArgs) = splitAt ar vals
          fName <- tmpName "result_" name
          emit $ B fName $ G.SApp name funArgs
          let applyArg n arg = do
                newName <- tmpName "result_" name
                emit (B newName $ G.SApp "apply" [G.Var n, arg])
                pure newName
          foldM_ applyArg fName extraArgs

remapNames :: Program -> [Name] -> CG Program
remapNames prog names = do
  env <- Map.fromList <$> sequence [(n,) <$> uniq n | n <- names]
  pure $ cata (\exp -> mapNameExp (subst env) $ embed exp) prog

genGrinMain :: CG G.Exp
genGrinMain = do
  fun_main <- uniq "fun_main"
  result_main <- uniq "result_main"
  ptr <- uniq "ptr"
  pure $
    G.Def "grinMain" [] $
      G.EBind (G.SApp "Main.main" []) (G.Var fun_main) $
      G.EBind (G.SApp "apply" [G.Var fun_main, G.Lit (G.LBool False {-FIXME: should be unit-})]) (G.Var result_main) $
      G.ECase (G.Var result_main) $
        [ G.Alt (G.NodePat (G.Tag G.C "GHC.Prim.Unit#") [ptr]) $ G.SApp "_prim_int_print" [G.Lit $ G.LInt64 0]
        , G.Alt G.DefaultPat $ G.SApp "_prim_int_print" [G.Lit $ G.LInt64 $ -1]
        ]


genProgram :: Program -> CG G.Program
genProgram prog' = do
  prog <- remapNames prog' ["apply", "eval", "grinMain"]
  let Program defs = prog
  grinDefs <- forM defs $ \(Def name args exp) -> do
    clearCmds
    genExp exp
    G.Def name args <$> cmdToExp

  evalFun <- lift $ genEval mempty "eval" grinDefs
  applyDef <- lift $ genApply mempty "apply" grinDefs
  grinMain <- genGrinMain
  pure $ G.Program $ grinMain : evalFun : applyDef : grinDefs

codegenGrin :: Program -> G.Program
codegenGrin exp = evalState (evalStateT (genProgram exp) emptyEnv) (mkNameEnv exp) where
  emptyEnv = Env
    { _arityMap   = buildArityMap exp
    , _whnfMap    = mempty
    , _commands   = []
    }

-- HINT: arity map for lambda
buildArityMap :: Program -> Map Name Int
buildArityMap (Program defs) = Map.fromList [(name, length args) | Def name args _ <- defs]
buildArityMap _ = error "invalid expression, program expected"
