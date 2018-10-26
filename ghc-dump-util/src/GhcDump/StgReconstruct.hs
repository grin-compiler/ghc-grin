{-# LANGUAGE RecordWildCards, LambdaCase #-}
module GhcDump.StgReconstruct (reconModule) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

import GhcDump_StgAst


instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

data BinderMap
  = BinderMap
  { bmModule  :: ModuleName
  , bmMap     :: HM.HashMap BinderId Binder
  }

insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder b (BinderMap n m) = BinderMap n $ HM.insert (binderId b) b m

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder (BinderMap _ m) bid
  | Just b <- HM.lookup bid m = b
  | otherwise                 = error $ "unknown binder "++ show bid ++ ":\nin scope:\n"
                                        ++ unlines (map (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList m))


-- "recon" == "reconstruct"

reconLocalBinder :: BinderMap -> SBinder -> Binder
reconLocalBinder (BinderMap n m) SBinder{..} = Binder sbinderName sbinderId n False -- HINT: local binders only

reconModule :: SModule -> Module
reconModule Module{..} = Module moduleName modulePhase binds exts cons
  where
    bm    = BinderMap moduleName $ HM.fromList [(binderId b, b) | b <- tops ++ concatMap snd (exts ++ cons)]
    binds = map reconTopBinding moduleTopBindings

    tops  = map (mkTopBinder moduleName) $ concatMap topBindings moduleTopBindings
    exts  = [(m, map (mkTopBinder m) l) | (m, l) <- moduleExternals]
    cons  = [(m, map (mkTopBinder m) l) | (m, l) <- moduleDataCons]

    mkTopBinder :: ModuleName -> SBinder -> Binder
    mkTopBinder m SBinder{..} = Binder sbinderName sbinderId m True

    reconTopBinder :: SBinder -> Binder
    reconTopBinder b = getBinder bm $ sbinderId b

    reconTopBinding :: STopBinding -> TopBinding
    reconTopBinding = \case
      StgTopStringLit b s           -> StgTopStringLit (reconTopBinder b) s
      StgTopLifted (StgNonRec b r)  -> StgTopLifted $ StgNonRec (reconTopBinder b) (reconRhs bm r)
      StgTopLifted (StgRec bs)      -> StgTopLifted $ StgRec [(reconTopBinder b, reconRhs bm r) | (b,r) <- bs]

topBindings :: TopBinding' bndr occ -> [bndr]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l            -> StgLit l
  StgLam bs x         -> let bs'   = map (reconLocalBinder bm) bs
                             bm'  = insertBinders bs' bm
                         in StgLam bs' (reconExpr bm' x)
  StgCase x b alts    -> let b'   = reconLocalBinder bm b
                             bm'  = insertBinder b' bm
                         in StgCase (reconExpr bm x) b' (map (reconAlt bm') alts)
  StgApp f args       -> StgApp (getBinder bm f) (map (reconArg bm) args)
  StgOpApp op args    -> StgOpApp op (map (reconArg bm) args)
  StgConApp dc args   -> StgConApp (getBinder bm dc) (map (reconArg bm) args)
  StgLet b e          -> let (bm', b') = reconBinding bm b
                         in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e  -> let (bm', b') = reconBinding bm b
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
  StgRhsCon dc vs           -> StgRhsCon (getBinder bm dc) $ map (reconArg bm) vs
  StgRhsClosure b vs u bs e -> let bs'  = map (reconLocalBinder bm) bs
                                   bm'  = insertBinders bs' bm
                               in StgRhsClosure b (map (getBinder bm') vs) u bs' (reconExpr bm' e)

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
  AltDataCon dc -> AltDataCon $ getBinder bm dc
  AltLit l      -> AltLit l
  AltDefault    -> AltDefault
