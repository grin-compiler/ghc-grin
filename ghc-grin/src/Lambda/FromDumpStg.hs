{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Lambda.FromDumpStg (codegenLambda) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Text.Printf
import qualified Data.ByteString.Char8 as BS8

-- GHC Dump
import qualified GhcDump_StgAst as C

-- Lambda
import Lambda.Syntax (Name)
import Lambda.Syntax

type CG = StateT Env IO

data Env
  = Env
  { closureDefs :: [Def]
  , counter     :: Int
  , moduleName  :: String
  , dataCons    :: Set String
  , externals   :: Map C.BinderId String
--  , dataArity   :: Map String Int
--  , varArity    :: Map String Int
  }

convertUnique :: C.Unique -> String
convertUnique (C.Unique c i) = c : show i

genConName :: String -> C.Binder -> CG String
genConName p b = do
  modName <- gets moduleName
  let name = BS8.unpack . C.binderName $ b
      qualName = modName ++ "." ++ name
  modify $ \env@Env{..} -> env {dataCons = Set.insert (name) dataCons}
  pure name

genName :: String -> C.Binder -> CG String
genName p b = do
  modName <- gets (Map.lookup (C.binderId b) . externals) >>= \case
    Nothing -> gets moduleName
    Just n  -> pure n
  let name = BS8.unpack . C.binderName $ b
      qualName = modName ++ "." ++ name
  {-
  case C.unBndr b of
    C.Binder{..} -> do
      let C.IdInfo{..} = binderIdInfo
      when (binderIdDetails `elem` [C.DataConWorkId]) $ do
        modify $ \env@Env{..} -> env {dataCons = Set.insert (p ++ "-" ++ show idiArity ++ "-" ++ qualName) dataCons}
    _ -> pure ()
  -}
  pure qualName

{-
TODO - support these:
data Lit = MachNullAddr
         | MachLabel T_Text
         | LitInteger Integer
-}
convertLit :: C.Lit -> Lit
convertLit = \case
  C.MachInt     i -> LInt64 $ fromIntegral i
  C.MachInt64   i -> LInt64 $ fromIntegral i
  C.MachWord    w -> LWord64 $ fromIntegral w
  C.MachWord64  w -> LWord64 $ fromIntegral w
  C.MachFloat   f -> LFloat $ realToFrac f
  C.MachDouble  f -> LFloat $ realToFrac f
  C.MachStr     s -> LString s
  C.MachChar    c -> LChar c
  lit -> LError . show $ lit

visitAlt :: C.Alt -> CG Alt
visitAlt (C.Alt altCon argIds body) = do
  cpat <- case altCon of
    C.AltDataCon dc  -> NodePat <$> genConName "AltDataCon" dc <*> mapM (genName "alt") argIds
    C.AltLit lit     -> pure . LitPat $ convertLit lit
    C.AltDefault     -> pure DefaultPat
  Alt cpat <$> visitExpr body

visitArg :: C.Arg -> CG Exp
visitArg = \case
  C.StgVarArg o -> Var <$> genName "StgVarArg" o
  C.StgLitArg l -> pure . Lit $ convertLit l

visitExpr :: C.Expr -> CG Exp
visitExpr = \case
  C.StgLit lit          -> pure . Lit $ convertLit lit
  C.StgApp fun args     -> App <$> genName "StgApp" fun <*> mapM visitArg args
  C.StgConApp con args  -> Con <$> genConName "StgConApp" con <*> mapM visitArg args
  C.StgOpApp op args    -> App "_stg_op_TODO_" <$> mapM visitArg args -- TODO
  C.StgLam bs expr      -> Lam <$> mapM (genName "StgLam") bs <*> visitExpr expr
  C.StgCase expr result alts  -> do
    scrutName <- genName "StgCase" result
    scrutExp <- visitExpr expr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  C.StgLet (C.StgNonRec b r) e          -> Let <$> ((\name exp -> [(name, exp)]) <$> genName "StgLet-NonRec" b <*> visitRhs r) <*> visitExpr e
  C.StgLet (C.StgRec bs) e              -> LetRec <$> mapM (\(b,r) -> (,) <$> genName "StgLet-Rec" b <*> visitRhs r) bs <*> visitExpr e

  C.StgLetNoEscape (C.StgNonRec b r) e  -> Let <$> ((\name exp -> [(name, exp)]) <$> genName "StgLetNoEscape-NonRec" b <*> visitRhs r) <*> visitExpr e
  C.StgLetNoEscape (C.StgRec bs) e      -> LetRec <$> mapM (\(b,r) -> (,) <$> genName "StgLetNoEscape-Rec" b <*> visitRhs r) bs <*> visitExpr e

  expr -> error . printf "unsupported expr %s" $ show expr


visitRhs :: C.Rhs -> CG Exp
visitRhs = \case
  C.StgRhsClosure _ vs _ bs e -> Lam <$> mapM (genName "StgRhsClosure") bs <*> visitExpr e -- TODO: app vs
  C.StgRhsCon con args        -> Con <$> genConName "StgRhsCon" con <*> mapM visitArg args

visitTopBinder :: C.TopBinding -> CG [Def]
visitTopBinder = \case
  C.StgTopStringLit b s             -> sequence [Def <$> genName "top binder" b <*> pure [] <*> pure (Lit $ LString s)]
  C.StgTopLifted (C.StgNonRec b r)  -> sequence [Def <$> genName "top binder" b <*> pure [] <*> visitRhs r]
  C.StgTopLifted (C.StgRec bs)      -> sequence [Def <$> genName "top binder" b <*> pure [] <*> visitRhs r | (b,r) <- bs]

visitModule :: C.Module -> CG [Def]
visitModule C.Module{..} = concat <$> mapM visitTopBinder moduleTopBindings

codegenLambda :: C.Module -> IO Program
codegenLambda mod = do
  let modName   = BS8.unpack . C.getModuleName $ C.moduleName mod
      extNames  = Map.fromList [] --[(C.binderId $ C.unBndr b, BS8.unpack $ C.getModuleName m) | C.ExternalName m b <- C.moduleExternals mod]
  (defs, Env{..}) <- runStateT (visitModule mod) (Env mempty 0 modName mempty extNames)
  unless (Set.null dataCons) $ do
    printf "%s data constructors:\n%s" modName  (unlines . map ("  "++) . Set.toList $ dataCons)
  pure . Program $ defs -- ++ closureDefs
