{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings #-}
module Lambda.ToStg (toStg) where

-- Compiler
import GHC
import DynFlags
import Outputable

-- Stg Types
import Module
import Name
import Id
import Unique
import OccName
import StgSyn
import CostCentre
import ForeignCall
import FastString
import BasicTypes
import CoreSyn (AltCon(..))

import PrimOp
import TysWiredIn
import TysPrim
import Literal
import MkId
import TyCon

import Control.Monad.State
import Data.List (partition)
import Data.Functor.Foldable

import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lambda.Syntax2 as L2

----------------
-- for test
import StgLoopback hiding (modl)
import qualified Data.ByteString as BS
import Data.Store

test :: IO ()
test = do
  let fname = "tsumupto.2.lambdabin"
  putStrLn $ "reading " ++ fname
  program <- decodeEx <$> BS.readFile fname :: IO L2.Exp
  rm <- readRepMap
  putStrLn "compiling"
  compileProgram NCG $ toStg rm program

----------------

{-
GHC.Prim.Unit#
lit:T_Int64
lit:T_Word64
lit:T_Int64
lit:T_Char

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Double  -- TODO: missing from GRIN
  | T_Bool
  | T_Unit
  | T_String
  | T_Char
  | T_Addr    -- TODO: missing from GRIN
  | T_Token   ByteString
-}

calcRep :: BS8.ByteString -> PrimRep
calcRep = \case
  "lit:T_Int64"     -> Int64Rep
  "lit:T_Word64"    -> Word64Rep
  "lit:T_Float"     -> FloatRep
  "lit:T_Double"    -> DoubleRep
  "lit:T_Char"      -> WordRep
  "lit:T_Addr"      -> AddrRep
  "GHC.Prim.Unit#"  -> UnliftedRep
  name
    | BS8.isPrefixOf "lit:T_Token" name -> VoidRep
    | BS8.isPrefixOf "GHC.Prim.(#" name -> UnliftedRep
    | otherwise                         -> LiftedRep

joinReps :: PrimRep -> PrimRep -> PrimRep
joinReps a b
 | a == b     = a
 | otherwise  = error $ "mismatching PrimReps: " ++ show a ++ " " ++ show b

readRepMap :: IO (Map L2.Name PrimRep)
readRepMap = do
  db <- map (BS8.split '\t') . BS8.lines <$> BS8.readFile "TagValue.csv"
  pure $ Map.unionsWith joinReps [Map.singleton (L2.packName $ BS8.unpack name) (calcRep rep) | [name, rep] <- db]

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

primOpMap :: Map L2.Name PrimOp
primOpMap = Map.fromList [(L2.packName . occNameString . primOpOcc $ op, op) | op <- allThePrimOps]

ambiguousPrimOps :: Map L2.Name [Int]
ambiguousPrimOps = Map.filter (\a -> length a > 1) $ Map.unionsWith (++) [Map.singleton (L2.packName . occNameString . primOpOcc $ op) [primOpTag op] | op <- allThePrimOps]

---------------

data Env
  = Env
  { topBindings     :: [StgTopBinding]
  , nameMap         :: Map L2.Name Unique
  , externalMap     :: Map L2.Name L2.External
  , ffiUniqueCount  :: Int
  , repMap          :: Map L2.Name PrimRep
  }

type StgM = State Env

freshFFIUnique :: StgM Unique
freshFFIUnique = state $ \env@Env{..} -> (mkUnique 'f' ffiUniqueCount, env {ffiUniqueCount = succ ffiUniqueCount})

addTopBinding :: StgTopBinding -> StgM ()
addTopBinding b = modify' $ \env@Env{..} -> env {topBindings = b : topBindings}

setExternals :: [L2.External] -> StgM ()
setExternals exts = modify' $ \env@Env{..} -> env {externalMap = Map.fromList [(L2.eName e, e) | e <- exts]}

getNameUnique :: L2.Name -> StgM Unique
getNameUnique n = state $ \env@Env{..} -> case Map.lookup n nameMap of
  Nothing -> (u, env {nameMap = Map.insert n u nameMap}) where u = mkUnique 'u' $ Map.size nameMap
  Just u  -> (u, env)

convertName :: L2.Name -> StgM Name
convertName n = do
  u <- getNameUnique n
  pure $ mkExternalName u modl (mkOccName OccName.varName $ L2.unpackName n) noSrcSpan

convertLiteral :: L2.Lit -> Literal
convertLiteral = \case
  L2.LInt64 a     -> LitNumber LitNumInt64  (fromIntegral a) int64PrimTy
  L2.LWord64 a    -> LitNumber LitNumWord64 (fromIntegral a) word64PrimTy
  L2.LFloat a     -> MachFloat a
  L2.LDouble a    -> MachDouble a
  L2.LChar a      -> MachChar a
  L2.LString a    -> MachStr a
  L2.LLabelAddr a -> MachLabel (mkFastString $ BS8.unpack a) (error "L2.LLabelAddr - arg size in bytes TODO") (error "L2.LLabelAddr - funcion or data TODO")
  L2.LNullAddr    -> MachNullAddr
  l               -> error $ "unsupported literal: " ++ show l


----------
-- type construction experiment

{-
primRepToRuntimeRep :: PrimRep -> Type
primRepToRuntimeRep rep = case rep of
  VoidRep       -> TyConApp tupleRepDataConTyCon [mkPromotedListTy runtimeRepTy []]
  LiftedRep     -> liftedRepDataConTy
  UnliftedRep   -> unliftedRepDataConTy
  IntRep        -> intRepDataConTy
  WordRep       -> wordRepDataConTy
  Int64Rep      -> int64RepDataConTy
  Word64Rep     -> word64RepDataConTy
  AddrRep       -> addrRepDataConTy
  FloatRep      -> floatRepDataConTy
  DoubleRep     -> doubleRepDataConTy
-}

primRepToType :: PrimRep -> Type
primRepToType = anyTypeOfKind . tYPE . primRepToRuntimeRep

getAltType :: L2.Name -> StgM AltType
getAltType _ = pure $ AlgAlt boolTyCon -- TODO

getType :: L2.Name -> StgM Type
getType _ = pure boolTy -- error "TODO: getType"

-- IMPORTANT: must provide enough information to the codegen for correct info table construction
getDataCon :: L2.Name -> [L2.Name] -> StgM DataCon
getDataCon con args = pure trueDataCon -- error "TODO: getDataCon"

----------

convertProgram :: L2.Program -> StgM ()
convertProgram (L2.Program exts sdata defs) = do
  setExternals exts
  mapM_ convertStaticData sdata
  mapM_ convertDef defs

convertStaticData :: L2.StaticData -> StgM ()
convertStaticData L2.StaticData{..} = case sValue of
  L2.StaticString s -> do
    n <- convertName sName
    addTopBinding $ StgTopStringLit (mkVanillaGlobal n addrPrimTy) s
convertDef :: L2.Def -> StgM ()
convertDef (L2.Def name args exp) = do
  name2 <- convertName name
  let ty = primRepToType LiftedRep -- TODO: is this OK?
  let nameId  = mkVanillaGlobal name2 ty
  stgArgs <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- args]
  exp2 <- convertExp exp
  addTopBinding $ StgTopLifted $ StgNonRec nameId $ StgRhsClosure dontCareCCS stgUnsatOcc [] Updatable stgArgs exp2

convertRHS :: L2.SimpleExp -> StgM StgRhs
convertRHS = \case
  L2.Con con args -> do
    dataCon <- getDataCon con args
    stgArgs <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- args]
    pure $ StgRhsCon dontCareCCS dataCon (map StgVarArg stgArgs)

  L2.Closure vars args body -> do
    stgVars <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- vars]
    stgArgs <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- args]
    body2 <- convertExp body
    pure $ StgRhsClosure dontCareCCS stgUnsatOcc stgVars Updatable stgArgs body2

  sexp -> error $ "invalid RHS simple exp " ++ show sexp

convertExp :: L2.Bind -> StgM StgExpr
convertExp = \case
  L2.Var name -> do
    name2 <- convertName name
    ty <- getType name
    pure $ StgApp (mkVanillaGlobal name2 ty) []

  L2.Let binds bind -> do
    bind2 <- convertExp bind
    binds2 <- forM binds $ \(name, sexp) -> do
      name2 <- convertName name
      ty <- getType name
      stgRhs <- convertRHS sexp
      pure $ StgNonRec (mkVanillaGlobal name2 ty) stgRhs
    pure $ foldr StgLet bind2 binds2

  L2.LetRec binds bind -> do
    bind2 <- convertExp bind
    binds2 <- forM binds $ \(name, sexp) -> do
      name2 <- convertName name
      ty <- getType name
      stgRhs <- convertRHS sexp
      pure (mkVanillaGlobal name2 ty, stgRhs)
    pure $ StgLet (StgRec binds2) bind2

  L2.LetS binds bind -> do
    bind2 <- convertExp bind
    binds2 <- forM binds $ \(name, sexp) -> do
      name2 <- convertName name
      ty <- getType name
      altKind <- getAltType name
      let nameId  = mkVanillaGlobal name2 ty
      sexp2 <- convertStrictExp nameId sexp
      pure (nameId, sexp2, altKind)

    let mkCase (nameId, exp, altKind) tailExp = StgCase exp nameId altKind [(DEFAULT, [], tailExp)]
    pure $ foldr mkCase bind2 binds2

convertStrictExp :: Id -> L2.SimpleExp -> StgM StgExpr
convertStrictExp resultId = \case
  L2.App name args -> do
    name2 <- convertName name
    stgArgs <- forM args $ \a -> StgVarArg <$> (mkVanillaGlobal <$> convertName a <*> getType a)
    extMap <- gets externalMap
    case Map.lookup name extMap of
      Nothing -> StgApp <$> (mkVanillaGlobal name2 <$> getType name) <*> pure stgArgs
      Just L2.External{..} -> case eKind of
        L2.PrimOp -> case Map.lookup name primOpMap of
          Nothing -> error $ "unknown primop: " ++ show (L2.unpackName name)
          Just op -> pure $ StgOpApp (StgPrimOp op) stgArgs (error "primop result type" :: Type) -- TODO: result type
        L2.FFI -> do
          let callSpec = CCallSpec (StaticTarget NoSourceText (mkFastString $ L2.unpackName name) Nothing True) CCallConv PlayRisky
          u <- freshFFIUnique
          pure $ StgOpApp (StgFCallOp (CCall callSpec) u) stgArgs (error "ffi result type" :: Type) -- TODO: result type

  L2.Con name args -> do
    dataCon <- getDataCon name args
    name2 <- convertName name
    stgArgs <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- args]
    types <- mapM getType args
    pure $ StgConApp dataCon (map StgVarArg stgArgs) types

  L2.Lit L2.LToken{} -> pure $ StgApp voidPrimId []
  L2.Lit l -> pure . StgLit $ convertLiteral l

  L2.Case name alts -> do
    name2 <- convertName name
    ty <- getType name
    altKind <- getAltType name
    let (defaultAlts, normalAlts) = partition (\(L2.Alt _ pat _) -> pat == L2.DefaultPat) alts
        stgVar = StgApp (mkVanillaGlobal name2 ty) []
    alts2 <- mapM convertAlt $ defaultAlts ++ normalAlts
    pure $ StgCase stgVar resultId altKind alts2

  exp -> convertExp exp

convertAlt :: L2.Alt -> StgM StgAlt
convertAlt (L2.Alt name pat bind) = do
  bind2 <- convertExp bind
  (altCon, params) <- case pat of
        L2.NodePat conName args -> do
          dataCon <- getDataCon conName args
          stgArgs <- sequence [mkVanillaGlobal <$> convertName a <*> getType a | a <- args]
          pure (DataAlt dataCon, stgArgs)
        L2.LitPat l -> pure (LitAlt $ convertLiteral l, [])
        L2.DefaultPat -> pure (DEFAULT, [])
  pure $ (altCon, params, bind2)

toStg :: Map L2.Name PrimRep -> L2.Program -> [StgTopBinding]
toStg rm prg = topBindings $ execState (convertProgram $ prepStg prg) emptyEnv where
  emptyEnv = Env
    { topBindings     = []
    , nameMap         = mempty
    , externalMap     = mempty
    , ffiUniqueCount  = 0
    , repMap          = rm
    }

prepStg :: L2.Program -> L2.Program
prepStg = cata folder where
  folder = \case
    L2.LetF binds bind    -> L2.LetS lits $ L2.Let nonLits bind    where (lits, nonLits) = partition isLitBind binds
    L2.LetRecF binds bind -> L2.LetS lits $ L2.LetRec nonLits bind where (lits, nonLits) = partition isLitBind binds
    exp -> embed exp

  isLitBind (_, L2.Lit{}) = True
  isLitBind _ = False
