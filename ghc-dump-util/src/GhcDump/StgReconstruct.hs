{-# LANGUAGE RecordWildCards, LambdaCase #-}
module GhcDump.StgReconstruct (reconModule) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

import GhcDump_StgAst hiding (ForeignCall)
--import GhcDump_Ast (BinderId(..), Binder(..), Unique(..), binderId, SBinder, ExternalName'(..), ExternalName, SExternalName)

--import GhcDump.Reconstruct (reconBinder, BinderMap, insertBinder, insertBinders, getBinder, emptyBinderMap, reconExternal, reconAltCon)

----
newtype BinderMap = BinderMap (HM.HashMap BinderId Binder)

instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

emptyBinderMap :: BinderMap
emptyBinderMap = BinderMap mempty

insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder b (BinderMap m) = BinderMap $ HM.insert (binderId b) b m

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder (BinderMap m) bid
  | Just b <- HM.lookup bid m = b
--  | otherwise                 = head $ HM.elems m -- HACK: until external names are not handled

  | otherwise                 = error $ "unknown binder "++ show bid ++ ":\nin scope:\n"
                                        ++ unlines (map (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList m))

----

-- "recon" == "reconstruct"

reconModule :: SModule -> Module
reconModule m = Module (moduleName m) (modulePhase m) binds [] [] -- TODO: exts
  where
    bm    = insertBinders (concatMap topBindings binds{- ++ [b | ExternalName _ b <- exts]-}) emptyBinderMap
    binds = map reconTopBinding $ moduleTopBindings m

    --exts  = map reconExternal $ moduleExternals m

    reconTopBinding :: STopBinding -> TopBinding
    reconTopBinding = \case
      StgTopLifted b      -> StgTopLifted (snd $ reconBinding bm b)
      StgTopStringLit b s -> StgTopStringLit b s

topBindings :: TopBinding' bndr occ -> [bndr]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l            -> StgLit l
  StgLam b x          -> let bm'  = insertBinders b bm
                         in StgLam b (reconExpr bm' x)
  StgCase x b alts    -> let bm'  = insertBinder b bm
                         in StgCase (reconExpr bm x) b (map (reconAlt bm') alts)
  StgApp f args       -> StgApp (getBinder bm f) (map (reconArg bm) args)
  StgOpApp op args    -> StgOpApp op (map (reconArg bm) args)
  StgConApp dc args   -> StgConApp (getBinder bm dc) (map (reconArg bm) args)
  StgLet b e          -> let (bm', b') = reconBinding bm b
                         in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e  -> let (bm', b') = reconBinding bm b
                         in StgLet b' (reconExpr bm' e)

reconBinding :: BinderMap -> SBinding -> (BinderMap, Binding)
reconBinding bm = \case
  StgNonRec b r -> let bm'  = insertBinder b bm
                   in (bm', StgNonRec b (reconRhs bm' r))

  StgRec bs     -> let bm'  = insertBinders (map fst bs) bm
                   in (bm', StgRec [(b, reconRhs bm' r) | (b,r) <- bs])

reconRhs :: BinderMap -> SRhs -> Rhs
reconRhs bm = \case
  StgRhsCon dc vs           -> StgRhsCon (getBinder bm dc) $ map (reconArg bm) vs
  StgRhsClosure b vs u bs e -> let bm'  = insertBinders bs bm
                               in StgRhsClosure b (map (getBinder bm') vs) u bs (reconExpr bm' e)

reconArg :: BinderMap -> SArg -> Arg
reconArg bm = \case
  StgVarArg b -> StgVarArg $ getBinder bm b
  StgLitArg l -> StgLitArg l

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm0 (Alt con bs rhs) =
    let bm'  = insertBinders bs bm0
    in Alt (reconAltCon bm0 con) bs (reconExpr bm' rhs)

reconAltCon :: BinderMap -> SAltCon -> AltCon
reconAltCon bm = \case
  AltDataCon dc -> AltDataCon $ getBinder bm dc
  AltLit l      -> AltLit l
  AltDefault    -> AltDefault
