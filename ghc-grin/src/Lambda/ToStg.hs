{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings #-}
module Lambda.ToStg (toStg, toStgWithNames) where

-- Compiler
import GHC
--import GHC.Driver.Session
import Outputable

-- Stg Types
import Module
import Name
import Id
import Unique
import OccName
import GHC.Stg.Syntax
--import StgSyn
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
import Type

import Control.Monad.State
import Data.List (partition)
import Data.Functor.Foldable

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Lambda.Syntax as L
import qualified Lambda.Name as L

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

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
  { topBindings     :: [(L.Name, StgTopBinding)]
  , nameMap         :: Map L.Name Unique
  , idMap           :: Map L.Name (Id, L.RepType)
  , externalMap     :: Map L.Name L.External
  , ffiUniqueCount  :: Int
  , dataConMap      :: Map L.Name (DataCon, [L.PrimRep])
  , tyCons          :: [TyCon]
  , moduleMap       :: Map (ByteString, ByteString) Module
  , currentModule   :: Module
  , publicNames     :: Set L.Name
  , voidVarSet      :: Set L.Name
  }

type StgM = State Env

addVoidVar :: L.Name -> StgM ()
addVoidVar n = modify' $ \env@Env{..} -> env {voidVarSet = Set.insert n voidVarSet}

clearVoidVarSet :: StgM ()
clearVoidVarSet = modify' $ \env@Env{..} -> env {voidVarSet = Set.empty}

freshFFIUnique :: StgM Unique
freshFFIUnique = state $ \env@Env{..} -> (mkUnique 'f' ffiUniqueCount, env {ffiUniqueCount = succ ffiUniqueCount})

addTopBinding :: L.Name -> StgTopBinding -> StgM ()
addTopBinding n b = modify' $ \env@Env{..} -> env {topBindings = (n, b) : topBindings}

setExternals :: [L.External] -> StgM ()
setExternals exts = modify' $ \env@Env{..} -> env {externalMap = Map.fromList [(L.eName e, e) | e <- exts]}

getNameUnique :: L.Name -> StgM Unique
getNameUnique n = state $ \env@Env{..} -> case Map.lookup n nameMap of
  Nothing -> (u, env {nameMap = Map.insert n u nameMap}) where u = mkUnique 'u' $ Map.size nameMap
  Just u  -> (u, env)

convertModule :: String -> String -> StgM Module
convertModule unitId modName = do
  let modKey = (BS8.pack unitId, BS8.pack modName)
  mm <- gets moduleMap
  case Map.lookup modKey mm of
    Just m  -> pure m
    -- cache module
    Nothing -> do
      {-
        TODO:
          - set package unit id
          stringToUnitId :: String -> UnitId
      -}
      let m = mkModule (stringToUnitId unitId) (mkModuleName modName)
      modify' $ \env@Env{..} -> env {moduleMap = Map.insert modKey m moduleMap}
      pure m

-- NOTE: only for GHC binder name conversion, not for FFI and foreign library symbols
convertVarName :: L.Name -> StgM Name
convertVarName = convertQualifiedName
{-
convertVarName n = do
  pubs <- gets publicNames
  case Set.member n pubs of
    False -> do
      u <- getNameUnique n
      pure $ mkFCallName u (L.unpackName n)
    True  -> do
      convertQualifiedName n
-}
convertQualifiedName :: L.Name -> StgM Name
convertQualifiedName n = do
  u <- getNameUnique n
  (modl, name) <- case L.decodePackageQualifiedName n of
    Just (unitId, modName, locName) -> do
      m <- convertModule unitId modName
      pure (m, locName)
    Nothing -> do
      m <- gets currentModule
      pure (m, L.unpackName n)
  pure $ mkExternalName u modl (mkOccName OccName.varName name) noSrcSpan

convertFArgType :: L.Ty -> Type
convertFArgType = \case
  L.TyCon _ "Array#" _              -> mkArrayPrimTy intTy
  L.TyCon _ "MutableArray#" _       -> mkMutableArrayPrimTy realWorldTy intTy
  L.TyCon _ "SmallArray#" _         -> mkSmallArrayPrimTy intTy
  L.TyCon _ "SmallMutableArray#" _  -> mkSmallMutableArrayPrimTy realWorldTy intTy
  L.TyCon _ "ArrayArray#" _         -> mkArrayArrayPrimTy
  L.TyCon _ "MutableArrayArray#" _  -> mkMutableArrayArrayPrimTy realWorldTy
  L.TyCon _ "ByteArray#" _          -> byteArrayPrimTy
  L.TyCon _ "MutableByteArray#" _   -> mkMutableByteArrayPrimTy realWorldTy
  _ -> intTy -- HINT: the actual type does not matter

-- type
-- IMPORTANT: 64 bit GHC only!!!
getPrimRep :: L.PrimRep -> PrimRep
getPrimRep = \case
  L.VoidRep     -> VoidRep
  L.LiftedRep   -> LiftedRep
  L.UnliftedRep -> UnliftedRep
  L.Int8Rep     -> Int8Rep
  L.Int16Rep    -> Int16Rep
  L.Int32Rep    -> Int32Rep
  L.Int64Rep    -> IntRep
  L.Word8Rep    -> Word8Rep
  L.Word16Rep   -> Word16Rep
  L.Word32Rep   -> Word32Rep
  L.Word64Rep   -> WordRep
  L.AddrRep     -> AddrRep
  L.FloatRep    -> FloatRep
  L.DoubleRep   -> DoubleRep

getType :: L.RepType -> Type
getType = \case
  L.SingleValue L.VoidRep -> mkTupleTy Unboxed []
  L.SingleValue r   -> primRepToType $ getPrimRep r
  L.UnboxedTuple l  -> mkTupleTy Unboxed $ map (primRepToType . getPrimRep) l
  L.PolymorphicRep  -> mkInvForAllTy runtimeRep2TyVar
                        $ mkSpecForAllTys [openBetaTyVar]
                        $ mkTyVarTy openBetaTyVar
                        -- forall (r :: RuntimeRep) (b :: TYPE r). b

getTypeM :: L.RepType -> StgM Type
getTypeM = pure . getType

-- id
addVarId :: L.Name -> L.RepType -> StgM Id
addVarId name rt = do
  name2 <- convertVarName name
  -- TODO: what kind of id do we need?
  {-
    #globalvslocal#
      Global 'Id's and 'Var's are those that are imported or correspond
        to a data constructor, primitive operation, or record selectors.
      Local 'Id's and 'Var's are those bound within an expression
        (e.g. by a lambda) or at the top level of the module being compiled.
  -}
  --  mkExportedVanillaId is used for :Main.main
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
    tcName <- convertQualifiedName cgName
    dcNames <- mapM (convertQualifiedName . L.csName) cgCons
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
  L.LFloat a       -> LitFloat a
  L.LDouble a      -> LitDouble a
  L.LChar a        -> LitChar a
  L.LString a      -> LitString a
  L.LDataAddr a    -> LitLabel (mkFastString $ BS8.unpack a) Nothing IsData
  L.LCodeAddr a i  -> LitLabel (mkFastString $ BS8.unpack a) i IsFunction
  L.LNullAddr      -> LitNullAddr
  l                 -> error $ "unsupported literal: " ++ show l

convertProgram :: L.Program -> StgM ()
convertProgram L.Program{..} = do
  setExternals pExternals
  setDataCons pConstructors
  -- top string literals
  mapM_ convertStaticData pStaticData

  -- gerenerate RhsCons for all necessary data con
  mapM_ genRhsCon pConstructors

  -- top closures
  let closureDefs = filter (not . isTopCon) pDefinitions

  forM_ closureDefs $ \(L.Def n _ _) -> do
    -- register top level closure ids
    addVarId n $ L.SingleValue L.LiftedRep

  mapM_ convertClosureDef closureDefs

-- static data
convertStaticData :: L.StaticData -> StgM ()
convertStaticData L.StaticData{..} = case sValue of
  L.StaticString s -> do
    i <- addVarId sName $ L.SingleValue L.AddrRep
    addTopBinding sName $ StgTopStringLit i s

-- top rhs con
isTopCon :: L.Def -> Bool
isTopCon (L.Def name [] (L.LetS [(v1, _, L.Con con [])] (L.Var v2))) = name == con && v1 == v2
isTopCon _ = False

-- Q: is this an optimisation or requirement?
genRhsCon :: L.ConGroup -> StgM ()
genRhsCon L.ConGroup{..} = forM_ cgCons $ \L.ConSpec{..} -> when (csArgsRep == []) $ do
  nameId <- addVarId csName (L.SingleValue L.LiftedRep)
  dataCon <- getDataCon (L.SingleValue L.UnliftedRep) csName
  addTopBinding csName $ StgTopLifted $ StgNonRec nameId $ StgRhsCon dontCareCCS dataCon []

setCurrentModuleFromName :: L.Name -> StgM ()
setCurrentModuleFromName n = do
  case L.decodePackageQualifiedName n of
    Nothing -> do
      error $ "invalid qualified name: " ++ L.unpackName n
    Just (unitId, modName, _) -> do
      mod <- convertModule unitId modName
      modify' $ \env@Env{..} -> env {currentModule = mod}

-- top rhs closure
convertClosureDef :: L.Def -> StgM ()
convertClosureDef (L.Def name args exp) = do
  clearVoidVarSet
  nameId <- getVarId name
  -- set current module
  setCurrentModuleFromName name
  stgArgs <- mapM (uncurry addVarId) args
  exp2 <- convertExp exp
  addTopBinding name $ StgTopLifted $ StgNonRec nameId $ StgRhsClosure noExtFieldSilent dontCareCCS Updatable stgArgs exp2

-- rhs
convertRHS :: L.SimpleExp -> StgM StgRhs
convertRHS = \case
  L.Con con args -> do
    dataCon <- getDataCon (L.SingleValue L.UnliftedRep) con
    stgArgs <- mapM getVarId args
    pure $ StgRhsCon dontCareCCS dataCon (map StgVarArg stgArgs)

  L.Closure _vars args body -> do
    --stgVars <- mapM getVarId vars
    stgArgs <- mapM (uncurry addVarId) args
    body2 <- convertExp body
    pure $ StgRhsClosure noExtFieldSilent dontCareCCS Updatable stgArgs body2

  sexp -> error $ "invalid RHS simple exp " ++ show sexp

data SExpRep
  = ValueRep
  | VoidVarRep
  | VoidValueRep
  | PolyVarRep
  | PolyValueRep
  deriving Eq

convertExp :: L.BindChain -> StgM StgExpr
convertExp = \case
  L.Var name -> do
    tokenVars <- gets voidVarSet
    case Set.member name tokenVars of
      True  -> pure $ StgApp voidPrimId []
      False -> do
        t <- getVarRepType name
        case t of
          L.PolymorphicRep        -> error $ "PolymorphicRep var: " ++ L.unpackName name
          L.UnboxedTuple []       -> pure $ StgConApp (tupleDataCon Unboxed 0) [] []
          L.SingleValue L.VoidRep -> pure $ StgConApp (tupleDataCon Unboxed 0) [] []
          L.SingleValue _ -> do
            i <- getVarId name
            pure $ StgConApp (tupleDataCon Unboxed 1) [StgVarArg i] []
          _ -> do
            i <- getVarId name
            pure $ StgApp i []

  L.Let binds bind -> do
    binds2 <- forM binds $ \(name, repType, sexp) -> do
      i <- addVarId name repType
      stgRhs <- convertRHS sexp
      pure $ StgNonRec i stgRhs
    bind2 <- convertExp bind
    pure $ foldr (StgLet noExtFieldSilent) bind2 binds2

  L.LetRec binds bind -> do
    -- register new Ids
    forM_ binds $ \(name, repType, _) -> do
      addVarId name repType
    binds2 <- forM binds $ \(name, _, sexp) -> do
      i <- getVarId name
      stgRhs <- convertRHS sexp
      pure (i, stgRhs)
    bind2 <- convertExp bind
    pure $ StgLet noExtFieldSilent (StgRec binds2) bind2
{-
  L.LetS [(name, repType, sexp)] (L.Case name2 alts) | name2 == name -> do
    nameId <- addVarId name repType
    sexp2 <- convertStrictExp repType nameId sexp
    let (defaultAlts, normalAlts) = partition (\(L.Alt _ pat _) -> pat == L.DefaultPat) alts
    altKind <- getPatternMatchAltType repType normalAlts
    alts2 <- mapM (convertAlt repType) $ defaultAlts ++ normalAlts
    case repType of
      L.UnboxedTuple [] -> do
        pure $ StgCase (StgConApp (tupleDataCon Unboxed 0) [] []) nameId altKind alts2
      _ -> do
        pure $ StgCase sexp2 nameId altKind alts2
-}


  -- filter void and (##) binds
  L.LetS [(name, _repType, L.Lit L.LToken{})] bind -> do
    addVoidVar name
    convertExp bind
  L.LetS [(name, L.SingleValue L.VoidRep, L.Var{})] bind -> do
    addVoidVar name
    convertExp bind
  L.LetS [(name, L.UnboxedTuple [], L.Var{})] bind -> do
    addVoidVar name
    convertExp bind

  -- var copy of unboxed tuple
  L.LetS [(name, repType@(L.UnboxedTuple _), sexp@(L.Var sname))] (L.Var name2) | name2 == name -> do
    rt <- getVarRepType sname
    case rt of
      L.SingleValue (L.LiftedRep) -> do
        nameId <- addVarId name repType
        convertStrictExp repType nameId sexp
      _ -> error $ "[case A] var copy of unboxed tuple, instruction: " ++ L.unpackName name

  L.LetS [(name, repType@(L.UnboxedTuple [_]), sexp)] (L.Var name2) | name2 == name -> do
    nameId <- addVarId name repType
    convertStrictExp repType nameId sexp

  L.LetS [(name, repType@(L.UnboxedTuple l), (L.Var name2))] bind | length l > 0 -> error $ "var copy of unboxed tuple, instruction: " ++ L.unpackName name

  -- var copy
  L.LetS [(name, repType@(L.SingleValue _), (L.Var name2))] bind -> do
    nameId <- addVarId name repType
    srcId <- getVarId name2
    bind2 <- convertExp bind
    let src = StgConApp (tupleDataCon Unboxed 1) [StgVarArg srcId] []
    --tmpId <- addVarId (L.packName "tmp") (L.UnboxedTuple [r])
    pure $ StgCase src srcId (MultiValAlt 1) [(DataAlt $ tupleDataCon Unboxed 1, [nameId], bind2)]

  -- last exp
  L.LetS [(name, repType, sexp)] (L.Var name2) | name2 == name -> do
    nameId <- addVarId name repType
    convertStrictExp repType nameId sexp

  -- no (##) bind
  L.LetS [(name, repType@(L.UnboxedTuple []), sexp)] bind -> do
    nameId <- addVarId name repType
    exp2 <- convertStrictExp repType nameId sexp
    bind2 <- convertExp bind
    let altKind = getEvalAltType repType
    pure $ StgCase exp2 nameId altKind [(DataAlt $ tupleDataCon Unboxed 0, [], bind2)]

  -- value binder, not the last exp
  L.LetS [(name, repType, sexp)] bind -> do
    nameId <- addVarId name repType
    exp2 <- convertStrictExp repType nameId sexp
    bind2 <- convertExp bind
    let altKind = getEvalAltType repType
    pure $ StgCase exp2 nameId altKind [(DEFAULT, [], bind2)]

convertStrictExp :: L.RepType -> Id -> L.SimpleExp -> StgM StgExpr
convertStrictExp resultTy resultId = \case
  L.App name args -> do
    tokenVars <- gets voidVarSet
    stgArgs <- forM args $ \a -> case Set.member a tokenVars of
      True  -> pure $ StgVarArg voidPrimId
      False -> StgVarArg <$> getVarId a
    extMap <- gets externalMap
    case Map.lookup name extMap of
      Nothing -> StgApp <$> getVarId name <*> pure stgArgs
      Just L.External{..} -> case eKind of
        L.PrimOp -> case Map.lookup name primOpMap of
          Just op -> pure $ StgOpApp (StgPrimOp op) stgArgs (idType resultId)
          Nothing -> case L.decodePackageQualifiedName name of
            Nothing -> error $ "invalid foreign primop name: " ++ L.unpackName name
            Just (uid, _, lbl) -> do
              -- foreign primop
              pure $ StgOpApp (StgPrimCallOp $ PrimCall (mkFastString lbl) (stringToUnitId uid)) stgArgs (idType resultId)
        L.FFI -> do
          let callSpec      = CCallSpec (StaticTarget NoSourceText (mkFastString $ L.unpackName name) Nothing True) CCallConv PlaySafe
              stgFCallType  = mkVisFunTys (map convertFArgType eArgsType) intTy -- HINT: the result type does not matter
          u <- freshFFIUnique
          pure $ StgOpApp (StgFCallOp (CCall callSpec) stgFCallType) stgArgs (idType resultId)

  L.Con name args -> do
    dataCon <- getDataCon resultTy name
    stgArgs <- mapM getVarId args
    pure $ StgConApp dataCon (map StgVarArg stgArgs) []

  L.Lit L.LToken{} -> error "LToken must not bind to STG variable" -- pure $ StgApp voidPrimId []
  L.Lit l -> pure . StgLit $ convertLiteral l

  L.Case name alts -> do
    let (defaultAlts, normalAlts) = partition (\(L.Alt _ pat _) -> pat == L.DefaultPat) alts
    sructRepType <- getVarRepType name
    altKind <- getPatternMatchAltType sructRepType normalAlts
    alts2 <- mapM (convertAlt sructRepType) $ defaultAlts ++ normalAlts
    case sructRepType of
      L.UnboxedTuple [] -> do
        pure $ StgCase (StgConApp (tupleDataCon Unboxed 0) [] []) resultId altKind alts2
      _ -> do
        scrutId <- getVarId name
        let stgVar = StgApp scrutId []
        pure $ StgCase stgVar scrutId altKind alts2

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
toStg = fmap (map snd) . toStgWithNames

toStgWithNames :: L.Program -> ([TyCon], [(L.Name, StgTopBinding)])
toStgWithNames prg = (tyCons, topBindings) where
  emptyEnv = Env
    { topBindings     = []
    , nameMap         = mempty
    , idMap           = mempty
    , externalMap     = mempty
    , ffiUniqueCount  = 0
    , dataConMap      = mempty
    , tyCons          = []
    , moduleMap       = Map.empty
    , currentModule   = mkModule mainUnitId (mkModuleName ":Main")
    , publicNames     = Set.fromList $ L.pPublicNames prg ++ L.pForeignExportedNames prg
    , voidVarSet      = Set.empty
    }
  Env{..} = execState (convertProgram $ prepStg prg) emptyEnv

prepStg :: L.Program -> L.Program
prepStg = cata folder where
  folder = \case
    L.LetF binds bind     -> foldr singleLetS (L.Let nonLits bind) lits     where (lits, nonLits) = partition isLitBind binds
    L.LetRecF binds bind  -> foldr singleLetS (L.LetRec nonLits bind) lits  where (lits, nonLits) = partition isLitBind binds
    L.LetSF binds bind    -> foldr singleLetS bind binds
    exp -> embed exp

  isLitBind (_, _, L.Lit{}) = True
  isLitBind _ = False

  singleLetS a b = L.LetS [a] b

{-
  STG invariants
    rule 1)
      - do not bind poly rep
      - all poly rep exp terminates bind chain
      MEANS: poly exp + its poly var is the result of closure or alt bind chain

    - do not bind void rep
    - do not bind (##)
    - bind chain ends with expression
        - non void rep Var
        - poly rep non var expression

  IMPORTANT: be avare of proper laziness conversion
    letS a = b IS NOT the same as Case b of a
    letS a = b IS Case (# b #) of {(# a #)}
-}
{-
  STG invarians
    - no void binder  - void var set
    - no (##) binders - 
    - proper lazyness conversion
    - no var bind chain terminator
-}