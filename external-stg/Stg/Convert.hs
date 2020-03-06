{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings, CPP #-}
module Stg.Convert where

import GhcPrelude

import qualified Data.ByteString.Char8 as BS8

import qualified CoreSyn      as GHC
import qualified DataCon      as GHC
import qualified FastString   as GHC
import qualified ForeignCall  as GHC
import qualified Id           as GHC
import qualified BasicTypes   as GHC
import qualified Literal      as GHC
import qualified Module       as GHC
import qualified Name         as GHC
import qualified Outputable   as GHC
import qualified PrimOp       as GHC
import qualified TyCon        as GHC
import qualified Type         as GHC
import qualified Unique       as GHC
import qualified GHC.Types.RepType  as GHC
import qualified GHC.Stg.Syntax     as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Driver.Types   as GHC

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IntMap (IntMap)
import Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set

import Stg.Syntax

import Debug.Trace
import Control.Exception
import System.IO.Unsafe
import qualified PprCore as GHC

--trace :: String -> a -> a
--trace _ = id

-- conversion env

data Env
  = Env
  { envExternalIds  :: IntMap GHC.Id
  , envAlgTyCons    :: IntMap GHC.TyCon
  , envQualPrefix   :: Name
  }

emptyEnv :: Env
emptyEnv = Env
  { envExternalIds  = IntMap.empty
  , envAlgTyCons    = IntMap.empty
  , envQualPrefix   = mempty
  }

type M = State Env

-- helpers

ppr :: GHC.Outputable a => a -> String
ppr = GHC.showSDoc GHC.unsafeGlobalDynFlags . GHC.ppr

bs8SDoc :: GHC.SDoc -> BS8.ByteString
bs8SDoc = BS8.pack . GHC.showSDoc GHC.unsafeGlobalDynFlags

uniqueKey :: GHC.Uniquable a => a -> Int
uniqueKey = GHC.getKey . GHC.getUnique

cvtUnique :: GHC.Unique -> Unique
cvtUnique u = Unique a b
  where (a,b) = GHC.unpkUnique u

-- name conversion

cvtOccName :: GHC.OccName -> Name
cvtOccName = GHC.bytesFS . GHC.occNameFS

cvtUnitId :: GHC.UnitId -> UnitId
cvtUnitId = UnitId . GHC.bytesFS . GHC.unitIdFS

cvtModuleName :: GHC.ModuleName -> ModuleName
cvtModuleName = ModuleName . GHC.bytesFS . GHC.moduleNameFS

cvtUnitIdAndModuleName :: GHC.Module -> (UnitId, ModuleName)
cvtUnitIdAndModuleName m = (cvtUnitId $ GHC.moduleUnitId m, cvtModuleName $ GHC.moduleName m)

-- data con conversion

addTyCon :: GHC.TyCon -> M ()
addTyCon tc = do
  let tcKey = uniqueKey tc
  new <- IntMap.notMember tcKey <$> gets envAlgTyCons
  when new $ modify' $ \m@Env{..} -> m {envAlgTyCons = IntMap.insert tcKey tc envAlgTyCons}

cvtDataCon :: GHC.DataCon -> M DataConId
cvtDataCon dc = do
  addTyCon $ GHC.dataConTyCon dc
  pure . DataConId . cvtUnique . GHC.getUnique $ dc

-- type conversion

cvtModuleQualification :: UnitId -> ModuleName -> Name
cvtModuleQualification (UnitId uid) (ModuleName mod) = (encodeSpecialSymbol '_' uid) `mappend` "_" `mappend` mod

encodeSpecialSymbol :: Char -> Name -> Name
encodeSpecialSymbol sym = t2b . duplicate sym . b2t where
  b2t = BS8.unpack
  t2b = BS8.pack
  duplicate _ [] = []
  duplicate c (x:xs)
    | x == c    = c : c : duplicate c xs
    | otherwise = x : duplicate c xs

getQualifiedTypeName :: GHC.Type -> M (Maybe Name)
getQualifiedTypeName t = case GHC.tyConAppTyConPicky_maybe t of
  Nothing -> pure Nothing
  Just tc -> do
    when (GHC.isAlgTyCon tc) $ do
      addTyCon tc
    let name = GHC.getName tc
    qprefix <- case GHC.nameModule_maybe name of
      Nothing -> gets envQualPrefix
      Just m  -> do
        let (uid, mod) = cvtUnitIdAndModuleName m
        pure $ cvtModuleQualification uid mod
    pure . Just $ qprefix `mappend` "." `mappend` (encodeSpecialSymbol '.' . cvtOccName $ GHC.getOccName name)


instance GHC.Outputable Type where
  ppr = GHC.text . show

trpp :: (GHC.Outputable o, GHC.Outputable a) => String -> (o -> a) -> o -> a
trpp msg f a = trace (unwords [msg, ":"]) $
               trace ('\t' : ppr a) $
               trace (unwords ["\t\t=", ppr (f a), "\n-----------\n"]) $
               f a

{-# INLINE debugCvtAppType #-}
debugCvtAppType :: GHC.Id -> [GHC.StgArg] -> GHC.Type -> Type
debugCvtAppType f args ty = unsafePerformIO $ debugCvtAppTypeM f args ty

{-# INLINE debugCvtAppTypeM #-}
debugCvtAppTypeM :: GHC.Id -> [GHC.StgArg] -> GHC.Type -> IO Type
debugCvtAppTypeM f args ty = catch (let t = cvtTypeNormal ty in seq t (pure t)) $ \ex -> do
  putStrLn $ "cought exception during StgApp result type conversion"
  putStrLn "function:"
  putStrLn $ "  " ++ ppr f ++ " :: " ++ ppr (GHC.idType f)
  putStrLn "args:"
  forM_ args $ \a -> case a of
    GHC.StgVarArg o -> putStrLn $ "    " ++ ppr o ++ " :: " ++ ppr (GHC.idType o)
    GHC.StgLitArg l -> putStrLn $ "    " ++ ppr l ++ " :: " ++ ppr (GHC.literalType l)
  putStrLn $ "function result type:"
  putStrLn $ "  " ++ ppr ty
  throwIO (ex :: SomeException)

cvtType :: String -> GHC.Type -> Type
--cvtType msg t = trpp (unwords [msg, "cvtType"]) cvtType3 $ deepCheckType t
cvtType _ = cvtTypeNormal

deepCheckType :: GHC.Type -> GHC.Type
deepCheckType t = t -- TODO

cvtType3 :: GHC.Type -> Type
cvtType3 t
  | trpp "  isTypeLevPoly" GHC.isTypeLevPoly t
  = PolymorphicRep

  | trpp "  isUnboxedTupleType" GHC.isUnboxedTupleType t
  = UnboxedTuple []

  | otherwise
  = SingleValue LiftedRep

{-
cvtType msg t = trace
  ( "cvtType - " ++ msg ++
    " isPiTy: " ++ show (GHC.isPiTy t) ++
    " isDictLikeTy: " ++ (show $ GHC.isDictLikeTy t) ++
    " isTypeLevPoly: " ++ (show $ GHC.isTypeLevPoly t) ++
    " isUnboxedTupleType: " ++ (show $ GHC.isUnboxedTupleType t) ++
    " isFunTy: " ++ (show $ GHC.isFunTy t) ++
    " :: " ++ ppr t
  ) . cvtType2 $ t
-}
{-
isDictLikeTy :: Type -> Bool
-- Note [Dictionary-like types]

-- | Returns Just True if this type is surely lifted, Just False
-- if it is surely unlifted, Nothing if we can't be sure (i.e., it is
-- levity polymorphic), and panics if the kind does not have the shape
-- TYPE r.
isLiftedType_maybe :: HasDebugCallStack => Type -> Maybe Bool
-}

{-# INLINE cvtTypeNormal #-}
cvtTypeNormal :: GHC.Type -> Type
cvtTypeNormal t
  | GHC.isTypeLevPoly t
  = PolymorphicRep

  -- TODO: remove this test code case (only for an experiment)
  ---------------- begin -------
  | GHC.isUnboxedSumType t
  = error $ "unboxed sum are not supported yet! " ++ ppr t
  ---------------- end -------

  | GHC.isUnboxedTupleType t
  = UnboxedTuple (map cvtPrimRep $ GHC.typePrimRep t)

  | [rep] <- GHC.typePrimRepArgs t
  = SingleValue (cvtPrimRep rep)

  | otherwise
  = error $ "could not convert type: " ++ ppr t

cvtPrimRep :: GHC.PrimRep -> PrimRep
cvtPrimRep = \case
  GHC.VoidRep     -> VoidRep
  GHC.LiftedRep   -> LiftedRep
  GHC.UnliftedRep -> UnliftedRep
  GHC.Int8Rep     -> Int8Rep
  GHC.Int16Rep    -> Int16Rep
  GHC.Int32Rep    -> Int32Rep
  GHC.Int64Rep    -> Int64Rep
  GHC.IntRep      -> IntRep
  GHC.Word8Rep    -> Word8Rep
  GHC.Word16Rep   -> Word16Rep
  GHC.Word32Rep   -> Word32Rep
  GHC.Word64Rep   -> Word64Rep
  GHC.WordRep     -> WordRep
  GHC.AddrRep     -> AddrRep
  GHC.FloatRep    -> FloatRep
  GHC.DoubleRep   -> DoubleRep
  GHC.VecRep i e  -> VecRep i $ cvtPrimElemRep e

cvtPrimElemRep :: GHC.PrimElemRep -> PrimElemRep
cvtPrimElemRep = \case
  GHC.Int8ElemRep   -> Int8ElemRep
  GHC.Int16ElemRep  -> Int16ElemRep
  GHC.Int32ElemRep  -> Int32ElemRep
  GHC.Int64ElemRep  -> Int64ElemRep
  GHC.Word8ElemRep  -> Word8ElemRep
  GHC.Word16ElemRep -> Word16ElemRep
  GHC.Word32ElemRep -> Word32ElemRep
  GHC.Word64ElemRep -> Word64ElemRep
  GHC.FloatElemRep  -> FloatElemRep
  GHC.DoubleElemRep -> DoubleElemRep

-- literal conversion

cvtLitNumType :: GHC.LitNumType -> LitNumType
cvtLitNumType = \case
  GHC.LitNumInteger -> LitNumInteger
  GHC.LitNumNatural -> LitNumNatural
  GHC.LitNumInt     -> LitNumInt
  GHC.LitNumInt64   -> LitNumInt64
  GHC.LitNumWord    -> LitNumWord
  GHC.LitNumWord64  -> LitNumWord64

cvtLabelSpec :: Maybe Int -> GHC.FunctionOrData -> LabelSpec
cvtLabelSpec mi = \case
  GHC.IsFunction  -> FunctionLabel mi
  GHC.IsData      -> DataLabel

cvtLit :: GHC.Literal -> Lit
cvtLit = \case
  GHC.LitChar x       -> LitChar x
  GHC.LitString x     -> LitString x
  GHC.LitNullAddr     -> LitNullAddr
  GHC.LitFloat x      -> LitFloat x
  GHC.LitDouble x     -> LitDouble x
  GHC.LitLabel x i d  -> LitLabel (GHC.bytesFS  x) (cvtLabelSpec i d)
  GHC.LitNumber t i _ -> LitNumber (cvtLitNumType t) i

-- Id conversion

mkBinderId :: GHC.Uniquable a => a -> BinderId
mkBinderId = BinderId . cvtUnique . GHC.getUnique

cvtOccId :: GHC.Id -> M BinderId
cvtOccId x = do
  let name = GHC.getName x
  when (GHC.isExternalName name) $ do
    let key = uniqueKey x
    new <- IntMap.notMember key <$> gets envExternalIds
    when new $ modify' $ \m@Env{..} -> m {envExternalIds = IntMap.insert key x envExternalIds}
  pure $ mkBinderId x

cvtBinderIdClosureParam :: String -> GHC.Id -> SBinder
cvtBinderIdClosureParam msg v
  | GHC.isId v = SBinder
      { sbinderName     = cvtOccName $ GHC.getOccName v
      , sbinderId       = BinderId . cvtUnique . GHC.idUnique $ v
      , sbinderType     = SingleValue . cvtPrimRep . {-trpp (unwords [msg, "cvtBinderIdClosureParam", ppr v])-} GHC.typePrimRep1 $ GHC.idType v
      , sbinderTypeSig  = BS8.pack . ppr $ GHC.idType v
      }
  | otherwise = error $ "Type binder in STG: " ++ (show $ cvtOccName $ GHC.getOccName v)


cvtBinderId :: String -> GHC.Id -> SBinder
cvtBinderId msg v
  | GHC.isId v = SBinder
      { sbinderName     = cvtOccName $ GHC.getOccName v
      , sbinderId       = BinderId . cvtUnique . GHC.idUnique $ v
      , sbinderType     = cvtType (unwords [msg, "cvtBinderId", ppr v]) $ GHC.idType v
      , sbinderTypeSig  = BS8.pack . ppr $ GHC.idType v
      }
  | otherwise = error $ "Type binder in STG: " ++ (show $ cvtOccName $ GHC.getOccName v)

-- stg op conversion

cvtCCallTarget :: GHC.CCallTarget -> CCallTarget
cvtCCallTarget = \case
  GHC.StaticTarget _ t _ _  -> StaticTarget $ GHC.bytesFS t
  GHC.DynamicTarget         -> DynamicTarget

cvtCCallConv :: GHC.CCallConv -> CCallConv
cvtCCallConv = \case
  GHC.CCallConv           -> CCallConv
  GHC.CApiConv            -> CApiConv
  GHC.StdCallConv         -> StdCallConv
  GHC.PrimCallConv        -> PrimCallConv
  GHC.JavaScriptCallConv  -> JavaScriptCallConv

cvtSafety :: GHC.Safety -> Safety
cvtSafety = \case
  GHC.PlaySafe          -> PlaySafe
  GHC.PlayInterruptible -> PlayInterruptible
  GHC.PlayRisky         -> PlayRisky

cvtForeignCall :: GHC.ForeignCall -> ForeignCall
cvtForeignCall (GHC.CCall (GHC.CCallSpec t c s)) = ForeignCall (cvtCCallTarget t) (cvtCCallConv c) (cvtSafety s)

cvtOp :: GHC.StgOp -> StgOp
cvtOp = \case
  GHC.StgPrimOp o     -> StgPrimOp (cvtOccName $ GHC.primOpOcc o)
  GHC.StgPrimCallOp _ -> StgPrimCallOp PrimCall -- TODO
  GHC.StgFCallOp f _  -> StgFCallOp $ cvtForeignCall f

-- arg conversion

cvtArg :: GHC.StgArg -> M SArg
cvtArg = \case
  GHC.StgVarArg o -> StgVarArg <$> cvtOccId o
  GHC.StgLitArg l -> pure $ StgLitArg (cvtLit l)

-- alt conversion

cvtAlt :: GHC.StgAlt -> M SAlt
cvtAlt (con, bs, e) = Alt <$> cvtAltCon con <*> pure (map (cvtBinderId "Alt") bs) <*> cvtExpr e

cvtAltCon :: GHC.AltCon -> M SAltCon
cvtAltCon = \case
  GHC.DataAlt con -> AltDataCon <$> cvtDataCon con
  GHC.LitAlt l    -> pure . AltLit $ cvtLit l
  GHC.DEFAULT     -> pure $ AltDefault

-- stg expr conversion

cvtExpr :: GHC.StgExpr -> M SExpr
cvtExpr = \case
#ifdef EXT_STG_FOR_NON_PATCHED_GHC
  GHC.StgApp f ps           -> StgApp <$> cvtOccId f <*> mapM cvtArg ps <*> pure PolymorphicRep <*> pure mempty
#else
  GHC.StgApp f ps (tr,o)    -> StgApp <$> cvtOccId f <*> mapM cvtArg ps <*> pure ({-cvtType "StgApp" t-}debugCvtAppType f ps tr) <*> pure (BS8.pack . ppr $ GHC.idType f, BS8.pack $ ppr tr, BS8.pack o)
#endif
  GHC.StgLit l              -> pure $ StgLit (cvtLit l)
  GHC.StgConApp dc ps ts    -> StgConApp <$> cvtDataCon dc <*> mapM cvtArg ps <*> pure (map (cvtType "StgConApp") ts)
  GHC.StgOpApp o ps t       -> StgOpApp (cvtOp o) <$> mapM cvtArg ps <*> pure (cvtType "StgOpApp" t) <*> getQualifiedTypeName t
  GHC.StgCase e b _ al      -> StgCase <$> cvtExpr e <*> pure (cvtBinderId "StgCase" b) <*> mapM cvtAlt al
  GHC.StgLet _ b e          -> StgLet <$> cvtBind b <*> cvtExpr e
  GHC.StgLetNoEscape _ b e  -> StgLetNoEscape <$> cvtBind b <*> cvtExpr e
  GHC.StgTick _ e           -> cvtExpr e
  e                         -> error $ "invalid stg expression: " ++ ppr e

-- stg rhs conversion (heap objects)

cvtUpdateFlag :: GHC.UpdateFlag -> UpdateFlag
cvtUpdateFlag = \case
  GHC.ReEntrant   -> ReEntrant
  GHC.Updatable   -> Updatable
  GHC.SingleEntry -> SingleEntry

cvtRhs :: GHC.StgRhs -> M SRhs
cvtRhs = \case
  GHC.StgRhsClosure _ _ u bs e  -> StgRhsClosure (cvtUpdateFlag u) (map (cvtBinderIdClosureParam "StgRhsClosure") bs) <$> cvtExpr e
  GHC.StgRhsCon _ dc args       -> StgRhsCon <$> cvtDataCon dc <*> mapM cvtArg args

-- bind and top-bind conversion

cvtBind :: GHC.StgBinding -> M SBinding
cvtBind = \case
  GHC.StgNonRec b r -> StgNonRec (cvtBinderId "StgNonRec" b) <$> cvtRhs r
  GHC.StgRec    bs  -> StgRec <$> sequence [(cvtBinderId "StgRec" b,) <$> cvtRhs r | (b, r) <- bs]

cvtTopBind :: GHC.StgTopBinding -> M STopBinding
cvtTopBind = \case
  GHC.StgTopLifted b        -> StgTopLifted <$> cvtBind b
  GHC.StgTopStringLit b bs  -> pure $ StgTopStringLit (cvtBinderId "StgTopStringLit" b) bs

cvtTopBinds :: [GHC.StgTopBinding] -> M [STopBinding]
cvtTopBinds binds = mapM cvtTopBind binds

-- foreign stubs

cvtForeignStubs :: GHC.ForeignStubs -> ForeignStubs
cvtForeignStubs = \case
  GHC.NoStubs           -> NoStubs
  GHC.ForeignStubs h c  -> ForeignStubs (bs8SDoc $ GHC.pprCode GHC.CStyle h) (bs8SDoc $ GHC.pprCode GHC.CStyle c)

cvtForeignSrcLang :: GHC.ForeignSrcLang -> ForeignSrcLang
cvtForeignSrcLang = \case
  GHC.LangC       -> LangC
  GHC.LangCxx     -> LangCxx
  GHC.LangObjc    -> LangObjc
  GHC.LangObjcxx  -> LangObjcxx
  GHC.LangAsm     -> LangAsm
  GHC.RawObject   -> RawObject

-- module conversion
cvtModule :: GHC.CoreProgram -> GHC.CoreProgram -> String -> GHC.UnitId -> GHC.ModuleName -> [GHC.StgTopBinding] -> GHC.ForeignStubs -> [(GHC.ForeignSrcLang, FilePath)] -> SModule
cvtModule core prep_core phase unitId' modName' binds foreignStubs foreignFiles =
  Module
  { modulePhase           = BS8.pack phase
  , moduleUnitId          = unitId
  , moduleName            = modName
  , moduleDependency      = dependencies
  , moduleExternalTopIds  = externalIds
  , moduleAlgTyCons       = algTyCons
  , moduleExported        = exported
  , moduleTopBindings     = topBinds
  , moduleForeignStubs    = cvtForeignStubs foreignStubs
  , moduleForeignFiles    = [(cvtForeignSrcLang s, p) | (s, p) <- foreignFiles]
  , moduleCoreSrc         = bs8SDoc $ GHC.pprCoreBindings core
  , modulePrepCoreSrc     = bs8SDoc $ GHC.pprCoreBindings prep_core
  } where
      (topBinds, Env{..}) = runState (cvtTopBinds binds) initialEnv
      initialEnv          = emptyEnv {envQualPrefix = cvtModuleQualification unitId modName}
      stgTopIds           = concatMap topBindIds binds
      exported            = groupByUnitIdAndModule
                              [ ( maybe (unitId, modName) cvtUnitIdAndModuleName $ GHC.nameModule_maybe $ GHC.getName i
                                , mkBinderId i
                                )
                              | i <- stgTopIds
                              , GHC.isExportedId i
                              ]
      modName             = cvtModuleName modName'
      unitId              = cvtUnitId unitId'
      topKeys             = IntSet.fromList $ map uniqueKey stgTopIds
      -- external id means that it is defined in other module, not in this one
      externalIds         = groupByUnitIdAndModule [mkExternalName e | (k,e) <- IntMap.toList envExternalIds, IntSet.notMember k topKeys]
      algTyCons           = groupByUnitIdAndModule . map mkAlgTyCon $ IntMap.elems envAlgTyCons

      -- calculate dependencies
      externalTyCons      = [(cvtUnitIdAndModuleName m, ()) | m <- catMaybes $ map (GHC.nameModule_maybe . GHC.getName) $ IntMap.elems envAlgTyCons]
      dependencies        = map (fmap (map fst)) $ groupByUnitIdAndModule $ [((u, m), ()) | (u, ml) <- externalIds, (m, _) <- ml] ++ externalTyCons

-- utils

groupByUnitIdAndModule :: Ord b => [((UnitId, ModuleName), b)] -> [(UnitId, [(ModuleName, [b])])]
groupByUnitIdAndModule l =
  Map.toList . fmap (Map.toList . fmap Set.toList) $
  Map.unionsWith (Map.unionWith Set.union)
  [Map.singleton u (Map.singleton m (Set.singleton b)) | ((u, m), b) <- l]

mkExternalName :: GHC.Id -> ((UnitId, ModuleName), SBinder)
mkExternalName x = (cvtUnitIdAndModuleName . GHC.nameModule $ GHC.getName x, cvtBinderId "mkExternalName" x)

mkAlgTyCon :: GHC.TyCon -> ((UnitId, ModuleName), SAlgTyCon)
mkAlgTyCon tc = (cvtUnitIdAndModuleName $ GHC.nameModule x, b) where
  x = GHC.getName tc
  b = AlgTyCon
      { tcName      = cvtOccName $ GHC.getOccName x
      , tcDataCons  = map mkSDataCon . sortDataCons $ GHC.tyConDataCons tc
      }
  sortDataCons l = IntMap.elems $ IntMap.fromList [(GHC.dataConTag dc, dc) | dc <- l]

mkSDataCon :: GHC.DataCon -> SDataCon
mkSDataCon dc = SDataCon
  { sdcName   = cvtOccName $ GHC.getOccName n
  , sdcId     = DataConId . cvtUnique . GHC.getUnique $ n
  , sdcRep    = if GHC.isUnboxedTupleCon dc
                  then UnboxedTupleCon
                  else AlgDataCon $ concatMap (getConArgRep . dcpp "3" GHC.typePrimRepArgs) $ dcpp "2" GHC.dataConRepArgTys $ dcpp "1" id $ dc
  } where
      dcpp :: GHC.Outputable o => String -> (o -> a) -> o -> a
      dcpp _ f x = f x
      --dcpp msg f a = trace ("mkSDataCon " ++ msg ++ " : " ++ ppr a) $ f a
      n = GHC.getName dc
      getConArgRep = \case
        [GHC.VoidRep] -> [] -- HINT: drop VoidRep arguments, the STG constructor builder code also ignores them
        [r]           -> [cvtPrimRep r]
        r             -> error $ "data con " ++ ppr n ++ "has invalid argument representation: " ++ ppr r

topBindIds :: GHC.StgTopBinding -> [GHC.Id]
topBindIds = \case
  GHC.StgTopLifted (GHC.StgNonRec b _)  -> [b]
  GHC.StgTopLifted (GHC.StgRec bs)      ->  map fst bs
  GHC.StgTopStringLit b _               -> [b]
