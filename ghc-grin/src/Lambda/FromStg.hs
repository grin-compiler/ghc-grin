{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverloadedStrings #-}
module Lambda.FromStg (codegenLambda) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.State.Strict
import Text.Printf
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

-- GHC Dump
import qualified GhcDump_StgAst as C
import qualified GhcDump.StgPretty as P
import qualified Text.PrettyPrint.ANSI.Leijen as P

-- Lambda
import Lambda.Syntax (Name)
import Lambda.Syntax

import qualified Lambda.GHCPrimOps as GHCPrim

type CG = StateT Env IO

data Env
  = Env
  { moduleName  :: !Name
  , dataCons    :: !(Set Name)
  , externals   :: !(Map Name External)
  }

primMap :: Map Name External
primMap = Map.fromList [(eName, e) | e@External{..} <- prims] where
  Program prims _ = GHCPrim.primPrelude

convertUnique :: C.Unique -> Name
convertUnique (C.Unique c i) = packName $  c : show i

genConName :: C.Binder -> CG Name
genConName b = do
  let name  = packName . BS8.unpack $ C.binderUniqueName b
      mname = packName . BS8.unpack . C.getModuleName . C.binderModule $ b
  modName <- gets moduleName
  when (modName == mname) $ do
    modify $ \env@Env{..} -> env {dataCons = Set.insert name dataCons}
  pure name

addExternal :: External -> CG ()
addExternal ext = modify $ \env@Env{..} -> env {externals = Map.insert (eName ext) ext externals}

genName :: C.Binder -> CG Name
genName = pure . packName . BS8.unpack . C.binderUniqueName

isPointer :: C.Binder -> Bool
isPointer b = case C.tRep $ C.binderType b of
  Just [C.LiftedRep]   -> True
  Just [C.UnliftedRep] -> True
  _               -> False

litType :: C.Lit -> SimpleType
litType = \case
  C.MachChar{}    -> T_Char
  C.MachStr{}     -> T_Addr
  C.MachNullAddr  -> T_Addr
  C.MachFloat{}   -> T_Float
  C.MachDouble{}  -> T_Double
  C.MachLabel{}   -> T_Addr
  C.LitNumber t _ -> case t of
    C.LitNumInteger -> T_Int64
    C.LitNumNatural -> T_Int64
    C.LitNumInt     -> T_Int64
    C.LitNumInt64   -> T_Int64
    C.LitNumWord    -> T_Word64
    C.LitNumWord64  -> T_Word64

repType :: C.PrimRep -> Maybe SimpleType
repType = \case
  C.IntRep    -> Just T_Int64
  C.WordRep   -> Just T_Word64
  C.Int64Rep  -> Just T_Int64
  C.Word64Rep -> Just T_Word64
  C.AddrRep   -> Just T_Addr
  C.FloatRep  -> Just T_Float
  C.DoubleRep -> Just T_Double

  -- NOTE:
  --  1. FFI does not support thunks and boxed types
  --  2. VecRep is not supported yet
  _ -> Nothing

showTypeInfo :: C.TypeInfo -> String
showTypeInfo ti = BS8.unpack (C.tType ti) ++ case C.tRep ti of
  Nothing   -> ""
  Just rep  -> " " ++ show rep

showArgType :: C.Arg -> String
showArgType = \case
  C.StgLitArg l -> show $ litType l
  C.StgVarArg b -> showTypeInfo $ C.binderType b

ffiArgType :: C.Arg -> Maybe SimpleType
ffiArgType = \case
  C.StgLitArg l -> Just $ litType l
  C.StgVarArg b -> case C.tRep $ C.binderType b of
    Just [t]  -> repType t
    _         -> Nothing

ffiRetType :: C.TypeInfo -> Maybe Ty
ffiRetType ti = case filter (/= C.VoidRep) <$> C.tRep ti of
  Just []   -> Just $ TyCon "GHC.Prim.(##)" []
  Just [t]  -> TyCon "GHC.Prim.Unit#" . map TySimple <$> mapM repType [t]
  _         -> Nothing

isVoidRep :: C.TypeInfo -> Bool
isVoidRep ti = C.tRep ti == Just [C.VoidRep]

isVoidArg :: C.Arg -> Bool
isVoidArg = \case
  C.StgVarArg b -> isVoidRep $ C.binderType b
  _             -> False

convertLit :: C.Lit -> Lit
convertLit = \case
  C.LitNumber t i -> case t of
    C.LitNumInteger -> LInt64 $ fromIntegral i
    C.LitNumNatural -> LInt64 $ fromIntegral i
    C.LitNumInt     -> LInt64 $ fromIntegral i
    C.LitNumInt64   -> LInt64 $ fromIntegral i
    C.LitNumWord    -> LWord64 $ fromIntegral i
    C.LitNumWord64  -> LWord64 $ fromIntegral i
  C.MachFloat   f -> LFloat $ realToFrac f
  C.MachDouble  f -> LFloat $ realToFrac f
  C.MachStr     s -> LString s
  C.MachChar    c -> LChar c
  C.MachLabel   l -> LLabelAddr l
  C.MachNullAddr  -> LNullAddr

visitAlt :: C.Alt -> CG Alt
visitAlt (C.Alt altCon argIds body) = do
  cpat <- case altCon of
    C.AltDataCon dc  -> NodePat <$> genConName dc <*> mapM genName argIds
    C.AltLit lit     -> pure . LitPat $ convertLit lit
    C.AltDefault     -> pure DefaultPat
  Alt cpat <$> visitExpr body

visitArg :: C.Arg -> CG Exp
visitArg = \case
  C.StgVarArg o -> Var (isPointer o) <$> genName o
  C.StgLitArg l -> pure . Lit $ convertLit l

visitExpr :: C.Expr -> CG Exp
visitExpr = \case
  C.StgLit lit            -> pure . Lit $ convertLit lit
  C.StgApp fun []         -> Var (isPointer fun) <$> genName fun
  C.StgApp fun args       -> App <$> genName fun <*> mapM visitArg args
  C.StgConApp con args t  -> Con <$> genConName con <*> mapM visitArg args
  C.StgOpApp op args ty   -> do
    let realArgs = filter (not . isVoidArg) args
        ffiTys = do
          argsTy <- mapM ffiArgType realArgs
          retTy <- ffiRetType ty
          pure (retTy, map TySimple argsTy)
    case op of
      C.StgPrimOp prim -> genPrimOpWithFix prim args realArgs ty

      C.StgPrimCallOp _ -> pure . Lit $ LError "GHC PrimCallOp is not supported"

      C.StgFCallOp f@C.ForeignCall{..} -> case foreignCTarget of
        C.DynamicTarget -> do
          let (fnTy:argsTy) = map showArgType args
              retTy         = showTypeInfo ty
          pure . Lit . LError . T.pack $ "DynamicTarget is not supported: (" ++ fnTy ++ ") :: " ++ intercalate " -> " (argsTy ++ [retTy])
        C.StaticTarget labelName -> case ffiTys of
          Just (retTy, argsTy) -> do
            let name = packName $ BS8.unpack labelName
            addExternal External
              { eName       = name
              , eRetType    = retTy
              , eArgsType   = argsTy
              , eEffectful  = True
              }
            App name <$> mapM visitArg realArgs
          _ -> do
            let name    = BS8.unpack labelName
                argsTy  = map showArgType args
                retTy   = showTypeInfo ty
            pure . Lit . LError . T.pack $ "Unsupported foreign function type: " ++ name ++ " :: " ++ intercalate " -> " (argsTy ++ [retTy])

  C.StgCase expr result alts  -> do
    scrutName <- genName result
    scrutExp <- visitExpr expr
    case alts of
      -- NOTE: force convention in STG
      [C.Alt C.AltDefault [] rhsExpr] -> LetS [(scrutName, scrutExp)] <$> visitExpr rhsExpr
      -- normal case
      _ -> LetS [(scrutName, scrutExp)] . Case (Var (isPointer result) scrutName) <$> mapM visitAlt alts
  C.StgLet (C.StgNonRec b r) e          -> Let <$> ((\name exp -> [(name, exp)]) <$> genName b <*> visitRhs r) <*> visitExpr e
  C.StgLet (C.StgRec bs) e              -> LetRec <$> mapM (\(b,r) -> (,) <$> genName b <*> visitRhs r) bs <*> visitExpr e

  C.StgLetNoEscape (C.StgNonRec b r) e  -> Let <$> ((\name exp -> [(name, exp)]) <$> genName b <*> visitRhs r) <*> visitExpr e
  C.StgLetNoEscape (C.StgRec bs) e      -> LetRec <$> mapM (\(b,r) -> (,) <$> genName b <*> visitRhs r) bs <*> visitExpr e

  -- NOTE: STG must not contain lambdas
  C.StgLam bs expr -> Lam <$> mapM genName bs <*> visitExpr expr

  expr -> error . printf "unsupported expr %s" $ show expr


visitRhs :: C.Rhs -> CG Exp
visitRhs = \case
  C.StgRhsCon con args      -> Con <$> genConName con <*> mapM visitArg args
  C.StgRhsClosure [] _ bs e -> Lam <$> mapM genName bs <*> visitExpr e
  C.StgRhsClosure vs _ bs e -> AppExp <$> (Lam <$> mapM genName (vs' ++ bs) <*> visitExpr e) <*> mapM (\v -> Var (isPointer v) <$> genName v) vs' where vs' = Set.toList . Set.fromList $ vs

visitTopRhs :: C.Binder -> C.Rhs -> CG Def
visitTopRhs b = \case
  C.StgRhsClosure [] _ bs e -> Def <$> genName b <*> mapM genName bs <*> visitExpr e
  rhs                       -> Def <$> genName b <*> pure [] <*> visitRhs rhs

visitTopBinder :: C.TopBinding -> CG [Def]
visitTopBinder = \case
  C.StgTopStringLit b s             -> sequence [Def <$> genName b <*> pure [] <*> pure (Lit $ LString s)]
  C.StgTopLifted (C.StgNonRec b r)  -> sequence [visitTopRhs b r]
  C.StgTopLifted (C.StgRec bs)      -> mapM (uncurry visitTopRhs) bs

visitModule :: C.Module -> CG [Def]
visitModule C.Module{..} = concat <$> mapM visitTopBinder moduleTopBindings

codegenLambda :: C.Module -> IO Program
codegenLambda mod = do
  let modName   = packName . BS8.unpack . C.getModuleName $ C.moduleName mod
  (defs, Env{..}) <- runStateT (visitModule mod) (Env modName mempty mempty)
  {-
  unless (Set.null dataCons) $ do
    printf "%s data constructors:\n%s" modName  (unlines . map (("  "++).unpackName) . Set.toList $ dataCons)
  -}
  pure . Program (Map.elems externals) $ defs

------------------------------------------------------------
-- Workaround for higher order and other problematic primops
------------------------------------------------------------

genPrimOp :: BS8.ByteString -> [C.Arg] -> C.TypeInfo -> CG Exp
genPrimOp prim realArgs ti = do
  let name = packName (BS8.unpack prim)
  case Map.lookup name primMap of
    Nothing -> pure . Lit $ LError $ "Unsupported GHC primop: " <> T.pack (BS8.unpack prim ++ " return type: " ++ showTypeInfo ti)
    Just e  -> do
      addExternal e
      App name <$> mapM visitArg realArgs


genPrimOpWithFix :: BS8.ByteString -> [C.Arg] -> [C.Arg] -> C.TypeInfo -> CG Exp

-- wrapper like higher order functions
genPrimOpWithFix "clearCCS#"               [C.StgVarArg f, s]    realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "atomically#"             [C.StgVarArg f, s]    realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "maskAsyncExceptions#"    [C.StgVarArg f, s]    realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "maskUninterruptible#"    [C.StgVarArg f, s]    realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "unmaskAsyncExceptions#"  [C.StgVarArg f, s]    realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "catch#"                  [C.StgVarArg f, _, s] realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "catchRetry#"             [C.StgVarArg f, _, s] realArgs ti = App <$> genName f <*> mapM visitArg [s]
genPrimOpWithFix "catchSTM#"               [C.StgVarArg f, _, s] realArgs ti = App <$> genName f <*> mapM visitArg [s]

-- other
genPrimOpWithFix "mkWeak#"  [o, b, _, s] realArgs ti = genPrimOp "mkWeakNoFinalizer#" [o, b] ti

-- normal primop
genPrimOpWithFix prim args realArgs ti = genPrimOp prim realArgs ti
