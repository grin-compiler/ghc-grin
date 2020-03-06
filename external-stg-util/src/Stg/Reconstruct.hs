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

data BinderMap
  = BinderMap
  { bmUnitId      :: UnitId
  , bmModule      :: ModuleName
  , bmIdMap       :: HM.HashMap BinderId Binder
  , bmDataConMap  :: HM.HashMap DataConId DataCon
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
  , binderIsTop       = False
  , binderIsExported  = False
  }

reconDataCon :: UnitId -> ModuleName -> AlgTyCon -> SDataCon -> DataCon
reconDataCon u m tc SDataCon{..} = DataCon
  { dcName    = sdcName
  , dcId      = sdcId
  , dcUnitId  = u
  , dcModule  = m
  , dcRep     = sdcRep
  , dcTyCon   = tc
  }

reconAlgTyCon :: UnitId -> ModuleName -> SAlgTyCon -> AlgTyCon
reconAlgTyCon u m AlgTyCon{..} = tc where
  tc = AlgTyCon
    { tcName      = tcName
    , tcDataCons  = map (reconDataCon u m tc) tcDataCons
    }

reconModule :: SModule -> Module
reconModule Module{..} = mod where
  mod = Module
    { modulePhase           = modulePhase
    , moduleUnitId          = moduleUnitId
    , moduleName            = moduleName
    , moduleDependency      = moduleDependency
    , moduleExternalTopIds  = exts
    , moduleAlgTyCons       = algs
    , moduleExported        = moduleExported
    , moduleTopBindings     = binds
    , moduleForeignStubs    = moduleForeignStubs
    , moduleForeignFiles    = moduleForeignFiles
    , moduleCoreSrc         = moduleCoreSrc
    , modulePrepCoreSrc     = modulePrepCoreSrc
    }

  bm    = BinderMap
          { bmUnitId      = moduleUnitId
          , bmModule      = moduleName
          , bmIdMap       = HM.fromList [(binderId b, b) | b <- tops ++ concatMap snd (concatMap snd exts)]
          , bmDataConMap  = HM.fromList [(dcId dc, dc) | dc <- cons]
          }

  cons :: [DataCon]
  cons  = [ dc
          | (u, ml) <- algs
          , (m, algTyCons) <- ml
          , tc <- algTyCons
          , dc <- tcDataCons tc
          ]

  algs :: [(UnitId, [(ModuleName, [AlgTyCon])])]
  algs = [(u, [(m, map (reconAlgTyCon u m) l) | (m, l) <- ml]) | (u, ml) <- moduleAlgTyCons]

  binds :: [TopBinding]
  binds = map reconTopBinding moduleTopBindings

  modNameMap = HM.fromList [(b, (u, m)) | (u, ml) <- moduleExported, (m, l) <- ml, b <- l]

  tops :: [Binder]
  tops  = [ mkTopBinder unitId modName exported b
          | b@SBinder{..} <- concatMap topBindings moduleTopBindings
          , let (unitId, modName) = HM.lookupDefault (moduleUnitId, moduleName) sbinderId modNameMap
          , let exported          = HM.member sbinderId modNameMap
          ]

  exts :: [(UnitId, [(ModuleName, [Binder])])]
  exts = [(u, [(m, map (mkTopBinder u m True) l) | (m, l) <- ml]) | (u, ml) <- moduleExternalTopIds]

  mkTopBinder :: UnitId -> ModuleName -> Bool -> SBinder -> Binder
  mkTopBinder u m exported SBinder{..} =
    Binder
    { binderName        = sbinderName
    , binderId          = sbinderId
    , binderType        = sbinderType
    , binderTypeSig     = sbinderTypeSig
    , binderUnitId      = u
    , binderModule      = m
    , binderIsTop       = True
    , binderIsExported  = exported
    }

  reconTopBinder :: SBinder -> Binder
  reconTopBinder b = getBinder bm $ sbinderId b

  reconTopBinding :: STopBinding -> TopBinding
  reconTopBinding = \case
    StgTopStringLit b s           -> StgTopStringLit (reconTopBinder b) s
    StgTopLifted (StgNonRec b r)  -> StgTopLifted $ StgNonRec (reconTopBinder b) (reconRhs bm r)
    StgTopLifted (StgRec bs)      -> StgTopLifted $ StgRec [(reconTopBinder b, reconRhs bm r) | (b,r) <- bs]

topBindings :: TopBinding' idBnd idOcc dcOcc -> [idBnd]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l              -> StgLit l
  StgCase x b alts      -> let b'   = reconLocalBinder bm b
                               bm'  = insertBinder b' bm
                           in StgCase (reconExpr bm x) b' (map (reconAlt bm') alts)
  StgApp f args t s     -> StgApp (getBinder bm f) (map (reconArg bm) args) t s
  StgOpApp op args t tn -> StgOpApp op (map (reconArg bm) args) t tn
  StgConApp dc args t   -> StgConApp (getDataCon bm dc) (map (reconArg bm) args) t
  StgLet b e            -> let (bm', b') = reconBinding bm b
                           in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e    -> let (bm', b') = reconBinding bm b
                           in StgLet b' (reconExpr bm' e)

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
