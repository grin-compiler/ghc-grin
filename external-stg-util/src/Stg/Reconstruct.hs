{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Reconstruct (reconModule, topBindings) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

import Stg.Syntax

instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable DataConId where
    hashWithSalt salt (DataConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable TyConId where
    hashWithSalt salt (TyConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

data BinderMap
  = BinderMap
  { bmUnitId      :: UnitId
  , bmModule      :: ModuleName
  , bmIdMap       :: HM.HashMap BinderId Binder
  , bmDataConMap  :: HM.HashMap DataConId DataCon
  , bmTyConMap    :: HM.HashMap TyConId TyCon
  }

-- Id handling
insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder b bm@BinderMap{..} = bm {bmIdMap = HM.insert (binderId b) b bmIdMap}

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder BinderMap{..} bid = case HM.lookup bid bmIdMap of
  Just b  -> b
  Nothing -> error $ "unknown binder "++ show bid ++ ":\nin scope:\n" ++
              unlines (map (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList bmIdMap))

-- DataCon handling
insertDataCon :: DataCon -> BinderMap -> BinderMap
insertDataCon dc bm@BinderMap{..} = bm {bmDataConMap = HM.insert (dcId dc) dc bmDataConMap}

insertDataCons :: [DataCon] -> BinderMap -> BinderMap
insertDataCons dcs bm = foldl' (flip insertDataCon) bm dcs

getDataCon :: BinderMap -> DataConId -> DataCon
getDataCon BinderMap{..} bid = case HM.lookup bid bmDataConMap of
  Just b  -> b
  Nothing -> error $ "unknown data con "++ show bid ++ ":\nin scope:\n" ++
              unlines (map (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList bmDataConMap))

-- TyCon handling
getTyCon :: BinderMap -> TyConId -> TyCon
getTyCon BinderMap{..} i = case HM.lookup i bmTyConMap of
  Just b  -> b
  Nothing -> error $ "unknown ty con "++ show i ++ ":\nin scope:\n" ++
              unlines (map (\(i',b) -> show i' ++ "\t" ++ show b) (HM.toList bmTyConMap))



-- "recon" == "reconstruct"

reconLocalBinder :: BinderMap -> SBinder -> Binder
reconLocalBinder BinderMap{..} SBinder{..} = -- HINT: local binders only
  Binder
  { binderName        = sbinderName
  , binderId          = sbinderId
  , binderType        = sbinderType
  , binderTypeSig     = sbinderTypeSig
  , binderUnitId      = bmUnitId
  , binderModule      = bmModule
  , binderScope       = LocalScope
  }

reconDataCon :: UnitId -> ModuleName -> TyCon -> SDataCon -> DataCon
reconDataCon u m tc SDataCon{..} = DataCon
  { dcName    = sdcName
  , dcId      = sdcId
  , dcUnitId  = u
  , dcModule  = m
  , dcRep     = sdcRep
  , dcTyCon   = tc
  }

reconTyCon :: UnitId -> ModuleName -> STyCon -> TyCon
reconTyCon u m STyCon{..} = tc where
  tc = TyCon
    { tcName      = stcName
    , tcId        = stcId
    , tcUnitId    = u
    , tcModule    = m
    , tcDataCons  = map (reconDataCon u m tc) stcDataCons
    }

reconModule :: SModule -> Module
reconModule Module{..} = mod where
  mod = Module
    { modulePhase               = modulePhase
    , moduleUnitId              = moduleUnitId
    , moduleName                = moduleName
    , moduleForeignStubs        = moduleForeignStubs
    , moduleHasForeignExported  = moduleHasForeignExported
    , moduleDependency          = moduleDependency
    , moduleExternalTopIds      = exts
    , moduleTyCons              = tyConList
    , moduleTopBindings         = binds
    , moduleForeignFiles        = moduleForeignFiles
    , moduleCoreSrc             = moduleCoreSrc
    , modulePrepCoreSrc         = modulePrepCoreSrc
    , moduleStgSrc              = moduleStgSrc
    }

  bm = BinderMap
       { bmUnitId      = moduleUnitId
       , bmModule      = moduleName
       , bmIdMap       = HM.fromList [(binderId b, b) | b <- tops ++ concatMap snd (concatMap snd exts)]
       , bmDataConMap  = HM.fromList [(dcId dc, dc) | dc <- cons]
       , bmTyConMap    = HM.fromList [(tcId tc, tc) | tc <- tyCons]
       }

  tyCons :: [TyCon]
  tyCons = concatMap (concatMap snd . snd) tyConList

  cons :: [DataCon]
  cons = concatMap tcDataCons tyCons

  tyConList :: [(UnitId, [(ModuleName, [TyCon])])]
  tyConList = [(u, [(m, map (reconTyCon u m) l) | (m, l) <- ml]) | (u, ml) <- moduleTyCons]

  binds :: [TopBinding]
  binds = map reconTopBinding moduleTopBindings

  tops :: [Binder]
  tops  = [ mkTopBinder moduleUnitId moduleName sbinderScope b
          | b@SBinder{..} <- concatMap topBindings moduleTopBindings
          ]

  exts :: [(UnitId, [(ModuleName, [Binder])])]
  exts = [(u, [(m, map (mkTopBinder u m HaskellExported) l) | (m, l) <- ml]) | (u, ml) <- moduleExternalTopIds]

  mkTopBinder :: UnitId -> ModuleName -> Scope -> SBinder -> Binder
  mkTopBinder u m scope SBinder{..} =
    Binder
    { binderName        = sbinderName
    , binderId          = sbinderId
    , binderType        = sbinderType
    , binderTypeSig     = sbinderTypeSig
    , binderUnitId      = u
    , binderModule      = m
    , binderScope       = scope
    }

  reconTopBinder :: SBinder -> Binder
  reconTopBinder b = getBinder bm $ sbinderId b

  reconTopBinding :: STopBinding -> TopBinding
  reconTopBinding = \case
    StgTopStringLit b s           -> StgTopStringLit (reconTopBinder b) s
    StgTopLifted (StgNonRec b r)  -> StgTopLifted $ StgNonRec (reconTopBinder b) (reconRhs bm r)
    StgTopLifted (StgRec bs)      -> StgTopLifted $ StgRec [(reconTopBinder b, reconRhs bm r) | (b,r) <- bs]

topBindings :: TopBinding' idBnd idOcc dcOcc tcOcc -> [idBnd]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l              -> StgLit l
  StgCase x b at alts   -> let b'   = reconLocalBinder bm b
                               bm'  = insertBinder b' bm
                           in StgCase (reconExpr bm x) b' (reconAltType bm at) (map (reconAlt bm') alts)
  StgApp f args t s     -> StgApp (getBinder bm f) (map (reconArg bm) args) t s
  StgOpApp op args t tc -> StgOpApp op (map (reconArg bm) args) t (getTyCon bm <$> tc)
  StgConApp dc args t   -> StgConApp (getDataCon bm dc) (map (reconArg bm) args) t
  StgLet b e            -> let (bm', b') = reconBinding bm b
                           in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e    -> let (bm', b') = reconBinding bm b
                           in StgLetNoEscape b' (reconExpr bm' e)

reconBinding :: BinderMap -> SBinding -> (BinderMap, Binding)
reconBinding bm = \case
  StgNonRec b r -> let b'   = reconLocalBinder bm b
                       bm'  = insertBinder b' bm
                   in (bm', StgNonRec b' (reconRhs bm' r))
  StgRec bs     -> let bs'  = map (reconLocalBinder bm . fst) bs
                       bm'  = insertBinders bs' bm
                   in (bm', StgRec [(b, reconRhs bm' r) | ((_,r), b) <- zip bs bs'])

reconRhs :: BinderMap -> SRhs -> Rhs
reconRhs bm = \case
  StgRhsCon dc vs       -> StgRhsCon (getDataCon bm dc) $ map (reconArg bm) vs
  StgRhsClosure u bs e  -> let bs'  = map (reconLocalBinder bm) bs
                               bm'  = insertBinders bs' bm
                           in StgRhsClosure u bs' (reconExpr bm' e)

reconArg :: BinderMap -> SArg -> Arg
reconArg bm = \case
  StgVarArg b -> StgVarArg $ getBinder bm b
  StgLitArg l -> StgLitArg l

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm (Alt con bs rhs) =
    let bs' = map (reconLocalBinder bm) bs
        bm' = insertBinders bs' bm
    in Alt (reconAltCon bm con) bs' (reconExpr bm' rhs)

reconAltCon :: BinderMap -> SAltCon -> AltCon
reconAltCon bm = \case
  AltDataCon dc -> AltDataCon $ getDataCon bm dc
  AltLit l      -> AltLit l
  AltDefault    -> AltDefault

reconAltType :: BinderMap -> SAltType -> AltType
reconAltType bm = \case
  PolyAlt       -> PolyAlt
  MultiValAlt i -> MultiValAlt i
  PrimAlt r     -> PrimAlt r
  AlgAlt tc     -> AlgAlt $ getTyCon bm tc
