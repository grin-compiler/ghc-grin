{-# LANGUAGE RecordWildCards, LambdaCase #-}
module GhcDump.StgReconstruct (reconModule) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

import GhcDump_StgAst
import GhcDump_Ast (BinderId(..), Binder(..), Unique(..), binderId, SBinder)

import GhcDump.Reconstruct (reconBinder, BinderMap, insertBinder, insertBinders, getBinder, emptyBinderMap)

-- "recon" == "reconstruct"

reconModule :: SModule -> Module
reconModule m = Module (moduleName m) (modulePhase m) binds
  where
    bm    = insertBinders (concatMap topBindings binds) emptyBinderMap
    binds = map reconTopBinding $ moduleTopBindings m

    reconTopBinding :: STopBinding -> TopBinding
    reconTopBinding = \case
      StgTopLifted b      -> StgTopLifted (snd $ reconBinding bm b)
      StgTopStringLit b s -> StgTopStringLit (reconBinder bm b) s

topBindings :: TopBinding' bndr occ -> [bndr]
topBindings = \case
  StgTopLifted (StgNonRec b _)  -> [b]
  StgTopLifted (StgRec bs)      -> map fst bs
  StgTopStringLit b _           -> [b]

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l            -> StgLit l
  StgLam b x          -> let b'   = map (reconBinder bm) b
                             bm'  = insertBinders b' bm
                         in StgLam b' (reconExpr bm' x)
  StgCase x b alts    -> let b'   = reconBinder bm b
                             bm'  = insertBinder b' bm
                         in StgCase (reconExpr bm x) b' (map (reconAlt bm') alts)
  StgApp f args       -> StgApp (getBinder bm f) (map (reconArg bm) args)
  StgOpApp op args    -> StgOpApp op (map (reconArg bm) args)
  StgConApp dc args   -> StgConApp dc (map (reconArg bm) args)
  StgLet b e          -> let (bm', b') = reconBinding bm b
                         in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e  -> let (bm', b') = reconBinding bm b
                         in StgLet b' (reconExpr bm' e)

reconBinding :: BinderMap -> SBinding -> (BinderMap, Binding)
reconBinding bm = \case
  StgNonRec b r -> let b'   = reconBinder bm b
                       bm'  = insertBinder b' bm
                   in (bm', StgNonRec b' (reconRhs bm' r))

  StgRec bs     -> let b'   = map (reconBinder bm . fst) bs
                       bm'  = insertBinders b' bm
                   in (bm', StgRec [(b, reconRhs bm' r) | (b, (_,r)) <- zip b' bs])

reconRhs :: BinderMap -> SRhs -> Rhs
reconRhs bm = \case
  StgRhsCon d vs            -> StgRhsCon d $ map (reconArg bm) vs
  StgRhsClosure b vs u bs e -> let bs'   = map (reconBinder bm) bs
                                   bm'  = insertBinders bs' bm
                               in StgRhsClosure b (map (getBinder bm') vs) u bs' (reconExpr bm' e)

reconArg :: BinderMap -> SArg -> Arg
reconArg bm = \case
  StgVarArg b -> StgVarArg $ getBinder bm b
  StgLitArg l -> StgLitArg l

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm0 (Alt con bs rhs) =
    let (bm', bs') = doBinders bm0 [] bs
    in Alt con bs' (reconExpr bm' rhs)
  where
    doBinders bm acc []       = (bm, reverse acc)
    doBinders bm acc (b:rest) = doBinders bm' (b':acc) rest
      where
        b'  = reconBinder bm b
        bm' = insertBinder b' bm
