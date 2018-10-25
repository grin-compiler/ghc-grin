{-# LANGUAGE LambdaCase, TupleSections #-}
module GhcDump_StgConvert where

import qualified Data.ByteString.Char8 as BS8

import qualified CoreSyn    as GHC
import qualified DataCon    as GHC
import qualified FastString as GHC
import qualified Id         as GHC
import qualified IdInfo     as GHC
import qualified Module     as GHC
import qualified Name       as GHC
import qualified PrimOp     as GHC
import qualified StgSyn     as GHC
import qualified Unique     as GHC
import Literal (Literal(..))

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import GhcDump_StgAst as Ast

fastStringToText :: GHC.FastString -> T_Text
fastStringToText = GHC.fastStringToByteString

occNameToText :: GHC.OccName -> T_Text
occNameToText = GHC.fastStringToByteString . GHC.occNameFS

cvtModuleName :: GHC.ModuleName -> Ast.ModuleName
cvtModuleName = Ast.ModuleName . fastStringToText . GHC.moduleNameFS

cvtAltCon :: GHC.AltCon -> Ast.SAltCon
cvtAltCon = undefined -- TODO
{-
cvtAltCon (DataAlt altcon) = Ast.AltDataCon (fastStringToText . moduleNameFS . GHC.moduleName <$> (nameModule_maybe $ getName altcon)) $ occNameToText $ getOccName altcon
cvtAltCon (LitAlt l)       = Ast.AltLit $ cvtLit l
cvtAltCon DEFAULT          = Ast.AltDefault
-}

cvtLit :: Literal -> Ast.Lit
cvtLit l =
    case l of
      Literal.MachChar x -> Ast.MachChar x
      Literal.MachStr x -> Ast.MachStr x
      Literal.MachNullAddr -> Ast.MachNullAddr
      Literal.MachInt x -> Ast.MachInt x
      Literal.MachInt64 x -> Ast.MachInt64 x
      Literal.MachWord x -> Ast.MachWord x
      Literal.MachWord64 x -> Ast.MachWord64 x
      Literal.MachFloat x -> Ast.MachFloat x
      Literal.MachDouble x -> Ast.MachDouble x
      Literal.MachLabel x _ _ -> Ast.MachLabel $ fastStringToText  x
      Literal.LitInteger x _ -> Ast.LitInteger x

cvtIdInfo :: GHC.IdInfo -> Ast.IdInfo
cvtIdInfo i = IdInfo
  { idiArity      = GHC.arityInfo i
  , idiCallArity  = GHC.callArityInfo i
  }

cvtUnique :: GHC.Unique -> Ast.Unique
cvtUnique u = Ast.Unique a b
  where (a,b) = GHC.unpkUnique u


type M = State (IntMap GHC.Id)

idKey = GHC.getKey . GHC.idUnique

cvtVar :: GHC.Id -> M BinderId
cvtVar x = do
  let name = GHC.getName x
  when (GHC.isExternalName name) $ do -- TODO: refine filter to not include local top level binders
    let key = idKey x
    new <- IntMap.notMember key <$> get
    when new $ modify' $ \m -> IntMap.insert key x m
  pure . BinderId . cvtUnique . GHC.idUnique $ x

cvtBinder :: GHC.Id -> Binder
cvtBinder v
  | GHC.isId v = Binder
      { binderName      = occNameToText $ GHC.getOccName v
      , binderId        = BinderId . cvtUnique . GHC.idUnique $ v
      , binderIdInfo    = (cvtIdInfo $ GHC.idInfo v) -- {GhcDump_Ast.idiUnfolding = NoUnfolding}
      }
  | otherwise = error $ "Type binder in STG: " ++ (show $ occNameToText $ GHC.getOccName v)

cvtDataCon :: GHC.DataCon -> M BinderId
cvtDataCon x = do -- DataCon (occNameToText $ GHC.getOccName d)
  let name = GHC.getName x
  {-
  when (GHC.isExternalName name) $ do -- TODO: refine filter to not include local top level binders
    let key = idKey x
    new <- IntMap.notMember key <$> get
    when new $ modify' $ \m -> IntMap.insert key x m
  -}
  pure . BinderId . cvtUnique . GHC.getUnique $ x

cvtOp :: GHC.StgOp -> StgOp
cvtOp = \case
  GHC.StgPrimOp o     -> StgPrimOp (PrimOp . occNameToText $ GHC.primOpOcc o)
  GHC.StgPrimCallOp c -> StgPrimCallOp PrimCall -- TODO
  GHC.StgFCallOp f u  -> StgFCallOp Ast.ForeignCall (cvtUnique u) -- TODO

cvtArg :: GHC.StgArg -> M SArg
cvtArg = \case
  GHC.StgVarArg o -> StgVarArg <$> cvtVar o
  GHC.StgLitArg l -> pure $ StgLitArg (cvtLit l)

cvtBinderInfo :: GHC.StgBinderInfo -> BinderInfo
cvtBinderInfo i
  | GHC.satCallsOnly i  = SatCallsOnly
  | otherwise           = NoStgBinderInfo

cvtUpdateFlag :: GHC.UpdateFlag -> UpdateFlag
cvtUpdateFlag = \case
  GHC.ReEntrant   -> ReEntrant
  GHC.Updatable   -> Updatable
  GHC.SingleEntry -> SingleEntry

cvtAlt :: GHC.StgAlt -> M SAlt
cvtAlt (con, bs, e) = Alt (cvtAltCon con) (map cvtBinder bs) <$> cvtExpr e

cvtExpr :: GHC.StgExpr -> M SExpr
cvtExpr = \case
  GHC.StgApp f ps         -> StgApp <$> cvtVar f <*> mapM cvtArg ps
  GHC.StgLit l            -> pure $ StgLit (cvtLit l)
  GHC.StgConApp dc ps _   -> StgConApp <$> cvtDataCon dc <*> mapM cvtArg ps
  GHC.StgOpApp o ps _     -> StgOpApp (cvtOp o) <$> mapM cvtArg ps
  GHC.StgLam bs e         -> StgLam (map cvtBinder bs) <$> cvtExpr e
  GHC.StgCase e b _ al    -> StgCase <$> cvtExpr e <*> pure (cvtBinder b) <*> mapM cvtAlt al
  GHC.StgLet b e          -> StgLet <$> cvtBind b <*> cvtExpr e
  GHC.StgLetNoEscape b e  -> StgLetNoEscape <$> cvtBind b <*> cvtExpr e
  GHC.StgTick _ e         -> cvtExpr e

cvtRhs :: GHC.StgRhs -> M SRhs
cvtRhs = \case
  GHC.StgRhsClosure _ b fs u bs e -> StgRhsClosure (cvtBinderInfo b) <$> mapM cvtVar fs <*> pure (cvtUpdateFlag u) <*> pure (map cvtBinder bs) <*> cvtExpr e
  GHC.StgRhsCon _ dc args         -> StgRhsCon <$> cvtDataCon dc <*> mapM cvtArg args

cvtBind :: GHC.StgBinding -> M SBinding
cvtBind = \case
  GHC.StgNonRec b r -> StgNonRec (cvtBinder b) <$> cvtRhs r
  GHC.StgRec    bs  -> StgRec <$> sequence [(cvtBinder b,) <$> cvtRhs r | (b, r) <- bs]

cvtTopBind :: GHC.StgTopBinding -> M STopBinding
cvtTopBind = \case
  GHC.StgTopLifted b        -> StgTopLifted <$> cvtBind b
  GHC.StgTopStringLit b bs  -> pure $ StgTopStringLit (cvtBinder b) bs

cvtModule :: String -> GHC.ModuleName -> [GHC.StgTopBinding] -> Ast.SModule
cvtModule phase modName binds = Ast.Module (cvtModuleName modName) (BS8.pack phase) topBinds [] []{-extNames-} where -- TODO
  (topBinds, extNameSet)  = runState (mapM cvtTopBind binds) IntMap.empty
  extNames                = [] -- [mkExternalName e | (k,e) <- IntMap.toList extNameSet, IntSet.notMember k topKeys]
  topKeys                 = IntSet.fromList $ concatMap topBindKeys binds
{-
mkExternalName :: GHC.Id -> SExternalName
mkExternalName x = ExternalName
  { externalModuleName  = cvtModuleName $ GHC.moduleName $ GHC.nameModule $ GHC.getName x
  , externalBinder      = cvtBinder x
  }
-}
topBindKeys :: GHC.StgTopBinding -> [Int]
topBindKeys = \case
  GHC.StgTopLifted (GHC.StgNonRec b _)  -> [idKey b]
  GHC.StgTopLifted (GHC.StgRec bs)      -> map (idKey . fst) bs
  GHC.StgTopStringLit b _               -> [idKey b]
