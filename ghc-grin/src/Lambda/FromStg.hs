{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Lambda.FromStg (codegenLambda) where

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
  { moduleName  :: String
  , dataCons    :: Set String
  , moduleSalt  :: String
  }

convertUnique :: C.Unique -> String
convertUnique (C.Unique c i) = c : show i

genConName :: C.Binder -> CG String
genConName b = do
  let name  = BS8.unpack $ C.binderUniqueName b
      mname = BS8.unpack . C.getModuleName . C.binderModule $ b
  modName <- gets moduleName
  when (modName == mname) $ do
    modify $ \env@Env{..} -> env {dataCons = Set.insert name dataCons}
  pure name

genName :: C.Binder -> CG String
genName b
  | C.binderIsTop b = pure . BS8.unpack . C.binderUniqueName $ b
  | otherwise       = do
      s <- gets moduleSalt
      pure $ (BS8.unpack $ C.binderUniqueName b) ++ s

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
    C.AltDataCon dc  -> NodePat <$> genConName dc <*> mapM genName argIds
    C.AltLit lit     -> pure . LitPat $ convertLit lit
    C.AltDefault     -> pure DefaultPat
  Alt cpat <$> visitExpr body

visitArg :: C.Arg -> CG Exp
visitArg = \case
  C.StgVarArg o -> Var <$> genName o
  C.StgLitArg l -> pure . Lit $ convertLit l

visitExpr :: C.Expr -> CG Exp
visitExpr = \case
  C.StgLit lit          -> pure . Lit $ convertLit lit
  C.StgApp fun args     -> App <$> genName fun <*> mapM visitArg args
  C.StgConApp con args  -> Con <$> genConName con <*> mapM visitArg args
  C.StgOpApp op args    -> case op of
    C.StgPrimOp prim  -> App ("_prim_ghc_" ++ BS8.unpack prim) <$> mapM visitArg args
    C.StgPrimCallOp _ -> App "_stg_primCall_TODO_" <$> mapM visitArg args -- TODO
    C.StgFCallOp _ _  -> App "_stg_fCall_TODO_" <$> mapM visitArg args -- TODO

  C.StgLam bs expr      -> Lam <$> mapM genName bs <*> visitExpr expr
  C.StgCase expr result alts  -> do
    scrutName <- genName result
    scrutExp <- visitExpr expr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  C.StgLet (C.StgNonRec b r) e          -> Let <$> ((\name exp -> [(name, exp)]) <$> genName b <*> visitRhs r) <*> visitExpr e
  C.StgLet (C.StgRec bs) e              -> LetRec <$> mapM (\(b,r) -> (,) <$> genName b <*> visitRhs r) bs <*> visitExpr e

  C.StgLetNoEscape (C.StgNonRec b r) e  -> Let <$> ((\name exp -> [(name, exp)]) <$> genName b <*> visitRhs r) <*> visitExpr e
  C.StgLetNoEscape (C.StgRec bs) e      -> LetRec <$> mapM (\(b,r) -> (,) <$> genName b <*> visitRhs r) bs <*> visitExpr e

  expr -> error . printf "unsupported expr %s" $ show expr


visitRhs :: C.Rhs -> CG Exp
visitRhs = \case
  C.StgRhsClosure _ vs _ bs e -> Lam <$> mapM genName bs <*> visitExpr e -- TODO: app vs
  C.StgRhsCon con args        -> Con <$> genConName con <*> mapM visitArg args

visitTopBinder :: C.TopBinding -> CG [Def]
visitTopBinder = \case
  C.StgTopStringLit b s             -> sequence [Def <$> genName b <*> pure [] <*> pure (Lit $ LString s)]
  C.StgTopLifted (C.StgNonRec b r)  -> sequence [Def <$> genName b <*> pure [] <*> visitRhs r]
  C.StgTopLifted (C.StgRec bs)      -> sequence [Def <$> genName b <*> pure [] <*> visitRhs r | (b,r) <- bs]

visitModule :: C.Module -> CG [Def]
visitModule C.Module{..} = concat <$> mapM visitTopBinder moduleTopBindings

codegenLambda :: Int -> C.Module -> IO Program
codegenLambda salt mod = do
  let modName   = BS8.unpack . C.getModuleName $ C.moduleName mod
  (defs, Env{..}) <- runStateT (visitModule mod) (Env modName mempty (printf ".%d" salt))
  unless (Set.null dataCons) $ do
    printf "%s data constructors:\n%s" modName  (unlines . map ("  "++) . Set.toList $ dataCons)
  pure . Program $ defs
