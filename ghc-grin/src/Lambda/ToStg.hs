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
import DataCon

import Control.Monad.State
import Data.List (partition)
import Data.Functor.Foldable

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lambda.Syntax2 as L2

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

primOpMap :: Map L2.Name PrimOp
primOpMap = Map.fromList [(L2.packName . occNameString . primOpOcc $ op, op) | op <- allThePrimOps]

ambiguousPrimOps :: Map L2.Name [Int]
ambiguousPrimOps = Map.filter (\a -> length a > 1) $
  Map.unionsWith (++) [Map.singleton (L2.packName . occNameString . primOpOcc $ op) [primOpTag op] | op <- allThePrimOps]


-- minimalistic type construction for GHC/STG codegen
simpleDataCon :: TyCon -> Name -> [PrimRep] -> ConTag -> DataCon
simpleDataCon tc name args tag = mkDataCon
  name False (error "TyConRepName") [] [] [] [] [] [] []
  (map primRepToType args) (error "Original result type") (error "RuntimeRepInfo")
  tc tag [] fakeWorkerId NoDataConRep
  where
    fakeWorkerId = mkVanillaGlobal name (error "repTy LiftedRep")

simpleTyCon :: Name -> [DataCon] -> TyCon
simpleTyCon name dataCons = mkAlgTyCon name [] (error "Kind") [] Nothing [] (mkDataTyConRhs dataCons) (VanillaAlgTyCon (error "TyConRepName")) False

primRepToType :: PrimRep -> Type
primRepToType = anyTypeOfKind . tYPE . primRepToRuntimeRep

---------------

data Env
  = Env
  { topBindings     :: [StgTopBinding]
  , nameMap         :: Map L2.Name Unique
  , idMap           :: Map L2.Name (Id, L2.RepType)
  , externalMap     :: Map L2.Name L2.External
  , ffiUniqueCount  :: Int
  , dataConMap      :: Map L2.Name (DataCon, [L2.PrimRep])
  , tyCons          :: [TyCon]
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

-- type
-- IMPORTANT: 64 bit GHC only!!!
getPrimRep :: L2.PrimRep -> PrimRep
getPrimRep = \case
  L2.VoidRep      -> VoidRep
  L2.LiftedRep    -> LiftedRep
  L2.UnliftedRep  -> UnliftedRep
  L2.Int64Rep     -> IntRep
  L2.Word64Rep    -> WordRep
  L2.AddrRep      -> AddrRep
  L2.FloatRep     -> FloatRep
  L2.DoubleRep    -> DoubleRep

getType :: L2.RepType -> Type
getType = \case
  L2.SingleValue r  -> primRepToType $ getPrimRep r
  L2.UnboxedTuple l -> mkTupleTy Unboxed $ map (primRepToType . getPrimRep) l

getTypeM :: L2.RepType -> StgM Type
getTypeM = pure . getType

-- id
addVarId :: L2.Name -> L2.RepType -> StgM Id
addVarId name rt = do
  name2 <- convertName name
  let nameId = mkVanillaGlobal name2 (getType rt)
  modify' $ \env@Env{..} -> env {idMap = Map.insert name (nameId, rt) idMap}
  pure nameId

getVarId :: L2.Name -> StgM Id
getVarId name = do
  m <- gets idMap
  case Map.lookup name m of
    Nothing -> error $ "unknown Var: " ++ L2.unpackName name
    Just v  -> pure $ fst v

getVarRepType :: L2.Name -> StgM L2.RepType
getVarRepType name = do
  m <- gets idMap
  case Map.lookup name m of
    Nothing -> error $ "unknown Var: " ++ L2.unpackName name
    Just v  -> pure $ snd v

-- data con

setDataCons :: [L2.ConGroup] -> StgM ()
setDataCons conGroups = do
  forM_ conGroups $ \L2.ConGroup{..} -> do
    tcName <- convertName cgName
    dcNames <- mapM (convertName . L2.csName) cgCons
    let dataCons  = [(cs, simpleDataCon tyCon conName (map getPrimRep csArgsRep) tag) | (conName, cs@L2.ConSpec{..}, tag) <- zip3 dcNames cgCons [1..]]
        tyCon     = simpleTyCon tcName $ map snd dataCons
        dcMaps    = [Map.singleton csName (dc, csArgsRep) | (L2.ConSpec{..}, dc) <- dataCons]
    modify' $ \env@Env{..} -> env {dataConMap = Map.unions (dataConMap : dcMaps), tyCons = tyCon : tyCons}

getDataCon :: L2.Name -> StgM DataCon
getDataCon con = do
  m <- gets dataConMap
  case Map.lookup con m of
    Nothing -> error $ "unknown DataCon: " ++ L2.unpackName con
    Just dc -> pure $ fst dc

getDataConArgTypes :: L2.Name -> StgM [L2.PrimRep]
getDataConArgTypes con = do
  m <- gets dataConMap
  case Map.lookup con m of
    Nothing -> error $ "unknown DataCon: " ++ L2.unpackName con
    Just dc -> pure $ snd dc

-- alt
getEvalAltType :: L2.RepType -> AltType
getEvalAltType = \case
  L2.UnboxedTuple l -> MultiValAlt $ length l
  L2.SingleValue  r -> case r of
    L2.LiftedRep    -> PolyAlt
    L2.UnliftedRep  -> PolyAlt
    _               -> PrimAlt $ getPrimRep r

getPatternMatchAltType :: L2.RepType -> [L2.Alt] -> StgM AltType
getPatternMatchAltType rt alts = case rt of
  L2.UnboxedTuple l -> pure . MultiValAlt $ length l
  L2.SingleValue  r -> case r of
    L2.LiftedRep    -> algAlt alts
    L2.UnliftedRep  -> algAlt alts
    _               -> pure . PrimAlt $ getPrimRep r
  where
    algAlt ((L2.Alt _ (L2.NodePat con _) _) : _) = AlgAlt . dataConTyCon <$> getDataCon con
    algAlt _ = error "can not calculate AltType"

-- conversion

convertLiteral :: L2.Lit -> Literal
convertLiteral = \case
  L2.LInt64 a       -> LitNumber LitNumInt64  (fromIntegral a) intPrimTy
  L2.LWord64 a      -> LitNumber LitNumWord64 (fromIntegral a) wordPrimTy
  L2.LFloat a       -> MachFloat a
  L2.LDouble a      -> MachDouble a
  L2.LChar a        -> MachChar a
  L2.LString a      -> MachStr a
  L2.LDataAddr a    -> MachLabel (mkFastString $ BS8.unpack a) Nothing IsData
  L2.LCodeAddr a i  -> MachLabel (mkFastString $ BS8.unpack a) i IsFunction
  L2.LNullAddr      -> MachNullAddr
  l                 -> error $ "unsupported literal: " ++ show l

convertProgram :: L2.Program -> StgM ()
convertProgram (L2.Program exts cons sdata defs) = do
  setExternals exts
  setDataCons cons
  -- top string literals
  mapM_ convertStaticData sdata

  -- create top level ids
  forM_ defs $ \(L2.Def n _ _) -> do
    addVarId n $ L2.SingleValue L2.LiftedRep

  -- top cons
  let (conDefs, closureDefs) = partition isTopCon defs
  mapM_ convertConDef conDefs
  -- TODO: create the necessary top rhs cons for certain DataCons
  --        ignore the builtin and unboxed DataCons

  -- top closures
  mapM_ convertClosureDef closureDefs

-- static data
convertStaticData :: L2.StaticData -> StgM ()
convertStaticData L2.StaticData{..} = case sValue of
  L2.StaticString s -> do
    i <- addVarId sName $ L2.SingleValue L2.AddrRep
    addTopBinding $ StgTopStringLit i s

-- top rhs con
isTopCon :: L2.Def -> Bool
isTopCon (L2.Def name [] (L2.LetS [(v1, _, L2.Con con [])] (L2.Var v2))) = name == con && v1 == v2
isTopCon _ = False

-- Q: is this an optimisation or requirement?
convertConDef :: L2.Def -> StgM ()
convertConDef (L2.Def name [] _) = do
  nameId <- getVarId name
  dataCon <- getDataCon name
  addTopBinding $ StgTopLifted $ StgNonRec nameId $ StgRhsCon dontCareCCS dataCon []

-- top rhs closure
convertClosureDef :: L2.Def -> StgM ()
convertClosureDef (L2.Def name args exp) = do
  nameId <- getVarId name
  stgArgs <- mapM (uncurry addVarId) args
  exp2 <- convertExp exp
  addTopBinding $ StgTopLifted $ StgNonRec nameId $ StgRhsClosure dontCareCCS stgUnsatOcc [] Updatable stgArgs exp2

-- rhs
convertRHS :: L2.SimpleExp -> StgM StgRhs
convertRHS = \case
  L2.Con con args -> do
    dataCon <- getDataCon con
    stgArgs <- mapM getVarId args
    pure $ StgRhsCon dontCareCCS dataCon (map StgVarArg stgArgs)

  L2.Closure vars args body -> do
    stgVars <- mapM getVarId vars
    stgArgs <- mapM (uncurry addVarId) args
    body2 <- convertExp body
    pure $ StgRhsClosure dontCareCCS stgUnsatOcc stgVars Updatable stgArgs body2

  sexp -> error $ "invalid RHS simple exp " ++ show sexp

convertExp :: L2.BindChain -> StgM StgExpr
convertExp = \case
  L2.Var name -> do
    i <- getVarId name
    pure $ StgApp i []

  L2.Let binds bind -> do
    binds2 <- forM binds $ \(name, repType, sexp) -> do
      i <- addVarId name repType
      stgRhs <- convertRHS sexp
      pure $ StgNonRec i stgRhs
    bind2 <- convertExp bind
    pure $ foldr StgLet bind2 binds2

  L2.LetRec binds bind -> do
    -- register new Ids
    forM_ binds $ \(name, repType, _) -> do
      addVarId name repType
    binds2 <- forM binds $ \(name, _, sexp) -> do
      i <- getVarId name
      stgRhs <- convertRHS sexp
      pure (i, stgRhs)
    bind2 <- convertExp bind
    pure $ StgLet (StgRec binds2) bind2

  L2.LetS binds bind -> do
    binds2 <- forM binds $ \(name, repType, sexp) -> do
      nameId <- addVarId name repType
      let altKind = getEvalAltType repType
      sexp2 <- convertStrictExp nameId sexp
      pure (nameId, sexp2, altKind)

    bind2 <- convertExp bind

    let mkCase (nameId, exp, altKind) tailExp = StgCase exp nameId altKind [(DEFAULT, [], tailExp)]
    pure $ foldr mkCase bind2 binds2

convertStrictExp :: Id -> L2.SimpleExp -> StgM StgExpr
convertStrictExp resultId = \case
  L2.App name args -> do
    stgArgs <- forM args $ \a -> StgVarArg <$> getVarId a
    extMap <- gets externalMap
    case Map.lookup name extMap of
      Nothing -> StgApp <$> getVarId name <*> pure stgArgs
      Just L2.External{..} -> case eKind of
        L2.PrimOp -> case Map.lookup name primOpMap of
          Nothing -> error $ "unknown primop: " ++ show (L2.unpackName name)
          Just op -> pure $ StgOpApp (StgPrimOp op) stgArgs (idType resultId)
        L2.FFI -> do
          let callSpec = CCallSpec (StaticTarget NoSourceText (mkFastString $ L2.unpackName name) Nothing True) CCallConv PlayRisky
          u <- freshFFIUnique
          pure $ StgOpApp (StgFCallOp (CCall callSpec) u) stgArgs (idType resultId)

  L2.Con name args -> do
    dataCon <- getDataCon name
    stgArgs <- mapM getVarId args
    pure $ StgConApp dataCon (map StgVarArg stgArgs) (error "StgConApp type list")

  L2.Lit L2.LToken{} -> pure $ StgApp voidPrimId []
  L2.Lit l -> pure . StgLit $ convertLiteral l

  L2.Case name alts -> do
    let (defaultAlts, normalAlts) = partition (\(L2.Alt _ pat _) -> pat == L2.DefaultPat) alts
    sructRepType <- getVarRepType name
    altKind <- getPatternMatchAltType sructRepType normalAlts
    alts2 <- mapM convertAlt $ defaultAlts ++ normalAlts
    stgVar <- StgApp <$> getVarId name <*> pure []
    pure $ StgCase stgVar resultId altKind alts2

  exp -> convertExp exp

convertAlt :: L2.Alt -> StgM StgAlt
convertAlt (L2.Alt name pat bind) = do
  (altCon, params) <- case pat of
    L2.NodePat conName args -> do
      dataCon <- getDataCon conName
      dcArgTys <- getDataConArgTypes conName
      stgArgs <- mapM (uncurry addVarId) $ zip args (map L2.SingleValue dcArgTys)
      pure (DataAlt dataCon, stgArgs)
    L2.LitPat l -> pure (LitAlt $ convertLiteral l, [])
    L2.DefaultPat -> pure (DEFAULT, [])
  bind2 <- convertExp bind
  pure $ (altCon, params, bind2)

{-
  converts lambda program that is already in STG form:
    - lazy computation is wrapped in closures
    - let/letrec rhs is only: con, closure
    - rep type is set for all binding sites
-}
toStg :: L2.Program -> ([TyCon], [StgTopBinding])
toStg prg = (tyCons, topBindings) where
  emptyEnv = Env
    { topBindings     = []
    , nameMap         = mempty
    , idMap           = mempty
    , externalMap     = mempty
    , ffiUniqueCount  = 0
    , dataConMap      = mempty
    , tyCons          = []
    }
  Env{..} = execState (convertProgram $ prepStg prg) emptyEnv

prepStg :: L2.Program -> L2.Program
prepStg = cata folder where
  folder = \case
    L2.LetF binds bind    -> L2.LetS lits $ L2.Let nonLits bind    where (lits, nonLits) = partition isLitBind binds
    L2.LetRecF binds bind -> L2.LetS lits $ L2.LetRec nonLits bind where (lits, nonLits) = partition isLitBind binds
    exp -> embed exp

  isLitBind (_, _, L2.Lit{}) = True
  isLitBind _ = False
