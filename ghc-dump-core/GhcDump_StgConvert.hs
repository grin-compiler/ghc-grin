{-# LANGUAGE LambdaCase #-}
module GhcDump_StgConvert where

import qualified Data.ByteString.Char8 as BS8

import qualified Module
import qualified StgSyn as GHC
import qualified DataCon as GHC
import qualified Id as Id
import qualified Name as GHC
import qualified PrimOp as GHC

import GhcDump_Ast (SBinder(..), BinderId(..), Binder'(..))
import GhcDump_StgAst as Ast
import GhcDump_Convert
  ( cvtModuleName
  , cvtUnique
  , cvtAltCon
  , cvtLit
  , cvtType
  , cvtIdInfo
  , cvtIdDetails
  , occNameToText
  )

cvtVar :: Id.Id -> BinderId
cvtVar = BinderId . cvtUnique . Id.idUnique

cvtBinder :: Id.Id -> SBinder
cvtBinder v
  | Id.isId v = SBndr $ Binder
      { binderName      = occNameToText $ GHC.getOccName v
      , binderId        = cvtVar v
      , binderIdInfo    = cvtIdInfo $ Id.idInfo v
      , binderIdDetails = cvtIdDetails $ Id.idDetails v
      , binderType      = cvtType $ Id.idType v
      }
  | otherwise = SBndr $ TyBinder
      { binderName      = occNameToText $ GHC.getOccName v
      , binderId        = cvtVar v
      , binderKind      = cvtType $ Id.idType v
      }

cvtDataCon :: GHC.DataCon -> DataCon
cvtDataCon d = DataCon (occNameToText $ GHC.getOccName d)

cvtOp :: GHC.StgOp -> StgOp
cvtOp = \case
  GHC.StgPrimOp o     -> StgPrimOp (PrimOp . occNameToText $ GHC.primOpOcc o)
  GHC.StgPrimCallOp c -> StgPrimCallOp PrimCall -- TODO
  GHC.StgFCallOp f u  -> StgFCallOp ForeignCall (cvtUnique u) -- TODO

cvtArg :: GHC.StgArg -> SArg
cvtArg = \case
  GHC.StgVarArg o -> StgVarArg (cvtVar o)
  GHC.StgLitArg l -> StgLitArg (cvtLit l)

cvtBinderInfo :: GHC.StgBinderInfo -> BinderInfo
cvtBinderInfo i
  | GHC.satCallsOnly i  = SatCallsOnly
  | otherwise           = NoStgBinderInfo

cvtUpdateFlag :: GHC.UpdateFlag -> UpdateFlag
cvtUpdateFlag = \case
  GHC.ReEntrant   -> ReEntrant
  GHC.Updatable   -> Updatable
  GHC.SingleEntry -> SingleEntry

cvtAlt :: GHC.StgAlt -> SAlt
cvtAlt (con, bs, e) = Alt (cvtAltCon con) (map cvtBinder bs) (cvtExpr e)

cvtExpr :: GHC.StgExpr -> SExpr
cvtExpr = \case
  GHC.StgApp f ps         -> StgApp (cvtVar f) (map cvtArg ps)
  GHC.StgLit l            -> StgLit (cvtLit l)
  GHC.StgConApp d ps _    -> StgConApp (cvtDataCon d) (map cvtArg ps)
  GHC.StgOpApp o ps _     -> StgOpApp (cvtOp o) (map cvtArg ps)
  GHC.StgLam bs e         -> StgLam (map cvtBinder bs) (cvtExpr e)
  GHC.StgCase e b _ al    -> StgCase (cvtExpr e) (cvtBinder b) (map cvtAlt al)
  GHC.StgLet b e          -> StgLet (cvtBind b) (cvtExpr e)
  GHC.StgLetNoEscape b e  -> StgLetNoEscape (cvtBind b) (cvtExpr e)
  GHC.StgTick _ e         -> cvtExpr e

cvtRhs :: GHC.StgRhs -> SRhs
cvtRhs = \case
  GHC.StgRhsClosure _ b fs u bs e -> StgRhsClosure (cvtBinderInfo b) (map cvtVar fs) (cvtUpdateFlag u) (map cvtBinder bs) (cvtExpr e)
  GHC.StgRhsCon _ dc args         -> StgRhsCon (cvtDataCon dc) (map cvtArg args)

cvtBind :: GHC.StgBinding -> SBinding
cvtBind = \case
  GHC.StgNonRec b r -> StgNonRec (cvtBinder b) (cvtRhs r)
  GHC.StgRec    bs  -> StgRec [(cvtBinder b, cvtRhs r) | (b, r) <- bs]

cvtTopBind :: GHC.StgTopBinding -> STopBinding
cvtTopBind = \case
  GHC.StgTopLifted b        -> StgTopLifted (cvtBind b)
  GHC.StgTopStringLit b bs  -> StgTopStringLit (cvtBinder b) bs

cvtModule :: String -> Module.ModuleName -> [GHC.StgTopBinding] -> Ast.SModule
cvtModule phase modName binds = Ast.Module (cvtModuleName modName) (BS8.pack phase) (map cvtTopBind binds)
