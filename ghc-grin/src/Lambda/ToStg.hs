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
import qualified Lambda.Syntax as L

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

modl :: Module
modl = mkModule mainUnitId (mkModuleName ":Main")

primOpMap :: Map L.Name PrimOp
primOpMap = Map.fromList [(L.packName . occNameString . primOpOcc $ op, op) | op <- allThePrimOps]

ambiguousPrimOps :: Map L.Name [Int]
ambiguousPrimOps = Map.filter (\a -> length a > 1) $
  Map.unionsWith (++) [Map.singleton (L.packName . occNameString . primOpOcc $ op) [primOpTag op] | op <- allThePrimOps]


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
  , nameMap         :: Map L.Name Unique
  , idMap           :: Map L.Name (Id, L.RepType)
  , externalMap     :: Map L.Name L.External
  , ffiUniqueCount  :: Int
  , dataConMap      :: Map L.Name (DataCon, [L.PrimRep])
  , tyCons          :: [TyCon]
  }

type StgM = State Env

freshFFIUnique :: StgM Unique
freshFFIUnique = state $ \env@Env{..} -> (mkUnique 'f' ffiUniqueCount, env {ffiUniqueCount = succ ffiUniqueCount})

addTopBinding :: StgTopBinding -> StgM ()
addTopBinding b = modify' $ \env@Env{..} -> env {topBindings = b : topBindings}

setExternals :: [L.External] -> StgM ()
setExternals exts = modify' $ \env@Env{..} -> env {externalMap = Map.fromList [(L.eName e, e) | e <- exts]}

getNameUnique :: L.Name -> StgM Unique
getNameUnique n = state $ \env@Env{..} -> case Map.lookup n nameMap of
  Nothing -> (u, env {nameMap = Map.insert n u nameMap}) where u = mkUnique 'u' $ Map.size nameMap
  Just u  -> (u, env)

convertName :: L.Name -> StgM Name
convertName n = do
  u <- getNameUnique n
  pure $ mkExternalName u modl (mkOccName OccName.varName $ L.unpackName n) noSrcSpan

-- type
-- IMPORTANT: 64 bit GHC only!!!
getPrimRep :: L.PrimRep -> PrimRep
getPrimRep = \case
  L.VoidRep      -> VoidRep
  L.LiftedRep    -> LiftedRep
  L.UnliftedRep  -> UnliftedRep
  L.Int64Rep     -> IntRep
  L.Word64Rep    -> WordRep
  L.AddrRep      -> AddrRep
  L.FloatRep     -> FloatRep
  L.DoubleRep    -> DoubleRep

getType :: L.RepType -> Type
getType = \case
  L.SingleValue r  -> primRepToType $ getPrimRep r
  L.UnboxedTuple l -> mkTupleTy Unboxed $ map (primRepToType . getPrimRep) l

getTypeM :: L.RepType -> StgM Type
getTypeM = pure . getType

-- id
addVarId :: L.Name -> L.RepType -> StgM Id
addVarId name rt = do
  name2 <- convertName name
  let nameId = mkVanillaGlobal name2 (getType rt)
  modify' $ \env@Env{..} -> env {idMap = Map.insert name (nameId, rt) idMap}
  pure nameId

getVarId :: L.Name -> StgM Id
getVarId name = do
  m <- gets idMap
  case Map.lookup name m of
    Nothing -> error $ "unknown Var: " ++ L.unpackName name
    Just v  -> pure $ fst v

getVarRepType :: L.Name -> StgM L.RepType
getVarRepType name = do
  m <- gets idMap
  case Map.lookup name m of
    Nothing -> error $ "unknown Var: " ++ L.unpackName name
    Just v  -> pure $ snd v

-- data con

setDataCons :: [L.ConGroup] -> StgM ()
setDataCons conGroups = do
  forM_ conGroups $ \L.ConGroup{..} -> do
    tcName <- convertName cgName
    dcNames <- mapM (convertName . L.csName) cgCons
    let dataCons  = [(cs, simpleDataCon tyCon conName (map getPrimRep csArgsRep) tag) | (conName, cs@L.ConSpec{..}, tag) <- zip3 dcNames cgCons [1..]]
        tyCon     = simpleTyCon tcName $ map snd dataCons
        dcMaps    = [Map.singleton csName (dc, csArgsRep) | (L.ConSpec{..}, dc) <- dataCons]
    modify' $ \env@Env{..} -> env {dataConMap = Map.unions (dataConMap : dcMaps), tyCons = tyCon : tyCons}

getDataCon :: L.RepType -> L.Name -> StgM DataCon
getDataCon (L.UnboxedTuple l) _con = pure $ tupleDataCon Unboxed (length l)
getDataCon _conTy con = do
  m <- gets dataConMap
  case Map.lookup con m of
    Nothing -> error $ "unknown DataCon: " ++ L.unpackName con
    Just dc -> pure $ fst dc

getDataConArgTypes :: L.RepType -> L.Name -> StgM [L.PrimRep]
getDataConArgTypes (L.UnboxedTuple l) _con = pure $ l
getDataConArgTypes _conTy con = do
  m <- gets dataConMap
  case Map.lookup con m of
    Nothing -> error $ "unknown DataCon: " ++ L.unpackName con
    Just dc -> pure $ snd dc

-- alt
getEvalAltType :: L.RepType -> AltType
getEvalAltType = \case
  L.UnboxedTuple l -> MultiValAlt $ length l
  L.SingleValue  r -> case r of
    L.LiftedRep    -> PolyAlt
    L.UnliftedRep  -> PolyAlt
    _               -> PrimAlt $ getPrimRep r

getPatternMatchAltType :: L.RepType -> [L.Alt] -> StgM AltType
getPatternMatchAltType scrutTy alts = case scrutTy of
  L.UnboxedTuple l -> pure . MultiValAlt $ length l
  L.SingleValue  r -> case r of
    L.LiftedRep    -> algAlt alts
    L.UnliftedRep  -> algAlt alts
    _              -> pure . PrimAlt $ getPrimRep r
  where
    algAlt ((L.Alt _ (L.NodePat con _) _) : _) = AlgAlt . dataConTyCon <$> getDataCon scrutTy con
    algAlt _ = error "can not calculate AltType"

-- conversion

convertLiteral :: L.Lit -> Literal
convertLiteral = \case
  L.LInt64 a       -> LitNumber LitNumInt64  (fromIntegral a) intPrimTy
  L.LWord64 a      -> LitNumber LitNumWord64 (fromIntegral a) wordPrimTy
  L.LFloat a       -> MachFloat a
  L.LDouble a      -> MachDouble a
  L.LChar a        -> MachChar a
  L.LString a      -> MachStr a
  L.LDataAddr a    -> MachLabel (mkFastString $ BS8.unpack a) Nothing IsData
  L.LCodeAddr a i  -> MachLabel (mkFastString $ BS8.unpack a) i IsFunction
  L.LNullAddr      -> MachNullAddr
  l                 -> error $ "unsupported literal: " ++ show l

convertProgram :: L.Program -> StgM ()
convertProgram (L.Program exts cons sdata defs) = do
  setExternals exts
  setDataCons cons
  -- top string literals
  mapM_ convertStaticData sdata

  -- create top level ids
  forM_ defs $ \(L.Def n _ _) -> do
    addVarId n $ L.SingleValue L.LiftedRep

  -- top cons
  let (conDefs, closureDefs) = partition isTopCon defs
  mapM_ convertConDef conDefs
  -- TODO: create the necessary top rhs cons for certain DataCons
  --        ignore the builtin and unboxed DataCons

  -- top closures
  mapM_ convertClosureDef closureDefs

-- static data
convertStaticData :: L.StaticData -> StgM ()
convertStaticData L.StaticData{..} = case sValue of
  L.StaticString s -> do
    i <- addVarId sName $ L.SingleValue L.AddrRep
    addTopBinding $ StgTopStringLit i s

-- top rhs con
isTopCon :: L.Def -> Bool
isTopCon (L.Def name [] (L.LetS [(v1, _, L.Con con [])] (L.Var v2))) = name == con && v1 == v2
isTopCon _ = False

-- Q: is this an optimisation or requirement?
convertConDef :: L.Def -> StgM ()
convertConDef (L.Def name [] _) = do
  nameId <- getVarId name
  dataCon <- getDataCon (L.SingleValue L.UnliftedRep) name
  addTopBinding $ StgTopLifted $ StgNonRec nameId $ StgRhsCon dontCareCCS dataCon []

-- top rhs closure
convertClosureDef :: L.Def -> StgM ()
convertClosureDef (L.Def name args exp) = do
  nameId <- getVarId name
  stgArgs <- mapM (uncurry addVarId) args
  exp2 <- convertExp exp
  addTopBinding $ StgTopLifted $ StgNonRec nameId $ StgRhsClosure dontCareCCS stgUnsatOcc [] Updatable stgArgs exp2

-- rhs
convertRHS :: L.SimpleExp -> StgM StgRhs
convertRHS = \case
  L.Con con args -> do
    dataCon <- getDataCon (L.SingleValue L.UnliftedRep) con
    stgArgs <- mapM getVarId args
    pure $ StgRhsCon dontCareCCS dataCon (map StgVarArg stgArgs)

  L.Closure vars args body -> do
    stgVars <- mapM getVarId vars
    stgArgs <- mapM (uncurry addVarId) args
    body2 <- convertExp body
    pure $ StgRhsClosure dontCareCCS stgUnsatOcc stgVars Updatable stgArgs body2

  sexp -> error $ "invalid RHS simple exp " ++ show sexp

convertExp :: L.BindChain -> StgM StgExpr
convertExp = \case
  L.Var name -> do
    i <- getVarId name
    pure $ StgApp i []

  L.Let binds bind -> do
    binds2 <- forM binds $ \(name, repType, sexp) -> do
      i <- addVarId name repType
      stgRhs <- convertRHS sexp
      pure $ StgNonRec i stgRhs
    bind2 <- convertExp bind
    pure $ foldr StgLet bind2 binds2

  L.LetRec binds bind -> do
    -- register new Ids
    forM_ binds $ \(name, repType, _) -> do
      addVarId name repType
    binds2 <- forM binds $ \(name, _, sexp) -> do
      i <- getVarId name
      stgRhs <- convertRHS sexp
      pure (i, stgRhs)
    bind2 <- convertExp bind
    pure $ StgLet (StgRec binds2) bind2

  L.LetS binds bind -> do
    binds2 <- forM binds $ \(name, repType, sexp) -> do
      nameId <- addVarId name repType
      let altKind = getEvalAltType repType
      sexp2 <- convertStrictExp repType nameId sexp
      pure (nameId, sexp2, altKind)

    bind2 <- convertExp bind

    let mkCase (nameId, exp, altKind) tailExp = StgCase exp nameId altKind [(DEFAULT, [], tailExp)]
    pure $ foldr mkCase bind2 binds2

convertStrictExp :: L.RepType -> Id -> L.SimpleExp -> StgM StgExpr
convertStrictExp resultTy resultId = \case
  L.App name args -> do
    stgArgs <- forM args $ \a -> StgVarArg <$> getVarId a
    extMap <- gets externalMap
    case Map.lookup name extMap of
      Nothing -> StgApp <$> getVarId name <*> pure stgArgs
      Just L.External{..} -> case eKind of
        L.PrimOp -> case Map.lookup name primOpMap of
          Nothing -> error $ "unknown primop: " ++ show (L.unpackName name)
          Just op -> pure $ StgOpApp (StgPrimOp op) stgArgs (idType resultId)
        L.FFI -> do
          let callSpec = CCallSpec (StaticTarget NoSourceText (mkFastString $ L.unpackName name) Nothing True) CCallConv PlayRisky
          u <- freshFFIUnique
          pure $ StgOpApp (StgFCallOp (CCall callSpec) u) stgArgs (idType resultId)

  L.Con name args -> do
    dataCon <- getDataCon resultTy name
    stgArgs <- mapM getVarId args
    pure $ StgConApp dataCon (map StgVarArg stgArgs) (error "StgConApp type list")

  L.Lit L.LToken{} -> pure $ StgApp voidPrimId []
  L.Lit l -> pure . StgLit $ convertLiteral l

  L.Case name alts -> do
    let (defaultAlts, normalAlts) = partition (\(L.Alt _ pat _) -> pat == L.DefaultPat) alts
    sructRepType <- getVarRepType name
    altKind <- getPatternMatchAltType sructRepType normalAlts
    alts2 <- mapM (convertAlt sructRepType) $ defaultAlts ++ normalAlts
    stgVar <- StgApp <$> getVarId name <*> pure []
    pure $ StgCase stgVar resultId altKind alts2

  exp -> convertExp exp

convertAlt :: L.RepType -> L.Alt -> StgM StgAlt
convertAlt scrutTy (L.Alt name pat bind) = do
  (altCon, params) <- case pat of
    L.NodePat conName args -> do
      dataCon <- getDataCon scrutTy conName
      dcArgTys <- getDataConArgTypes scrutTy conName
      stgArgs <- mapM (uncurry addVarId) $ zip args (map L.SingleValue dcArgTys)
      pure (DataAlt dataCon, stgArgs)
    L.LitPat l -> pure (LitAlt $ convertLiteral l, [])
    L.DefaultPat -> pure (DEFAULT, [])
  bind2 <- convertExp bind
  pure $ (altCon, params, bind2)

{-
  converts lambda program that is already in STG form:
    - lazy computation is wrapped in closures
    - let/letrec rhs is only: con, closure
    - rep type is set for all binding sites
-}
toStg :: L.Program -> ([TyCon], [StgTopBinding])
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

prepStg :: L.Program -> L.Program
prepStg = cata folder where
  folder = \case
    L.LetF binds bind    -> L.LetS lits $ L.Let nonLits bind    where (lits, nonLits) = partition isLitBind binds
    L.LetRecF binds bind -> L.LetS lits $ L.LetRec nonLits bind where (lits, nonLits) = partition isLitBind binds
    exp -> embed exp

  isLitBind (_, _, L.Lit{}) = True
  isLitBind _ = False
