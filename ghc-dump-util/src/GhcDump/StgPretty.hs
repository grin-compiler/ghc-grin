{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcDump.StgPretty
  ( Pretty(..)
  , module GhcDump.StgPretty
  ) where

import GhcDump_StgAst

import Data.Ratio
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen

{-
data ForeignCall
data PrimCall = PrimCall -- T_Text T_Text
data UpdateFlag = ReEntrant | Updatable | SingleEntry
-}


smallRArrow :: Doc
smallRArrow = "->"

hang' :: Doc -> Int -> Doc -> Doc
hang' d1 n d2 = hang n $ sep [d1, d2]

comment :: Doc -> Doc
comment x = "{-" <+> x <+> "-}"

maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

ppRep :: [PrimRep] -> Doc
ppRep = comment . text . show

pprBinder :: Binder -> Doc
pprBinder b = (pretty . binderUniqueName $ b) <+> comment (text (show u) <> exported) <+> ppRep (binderRep b) where
  BinderId u  = binderId b
  exported    = if binderIsExported b then text " exported" else mempty

instance Pretty T_Text where
    pretty = text . BS.unpack

instance Pretty ModuleName where
    pretty = text . BS.unpack . getModuleName

pprRational :: Rational -> Doc
pprRational r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty LitNumType where
  pretty = \case
    LitNumInteger -> "Integer"
    LitNumNatural -> "Natural"
    LitNumInt     -> "Int"
    LitNumInt64   -> "Int64"
    LitNumWord    -> "Word"
    LitNumWord64  -> "Word64"

instance Pretty Lit where
    pretty (MachChar x) = "'" <> char x <> "'#"
    pretty (MachStr x) = "\"" <> text (BS.unpack x) <> "\"#"
    pretty MachNullAddr = "nullAddr#"
    pretty (MachFloat x) = "FLOAT" <> parens (pprRational x)
    pretty (MachDouble x) = "DOUBLE" <> parens (pprRational x)
    pretty (MachLabel x) = "LABEL"<> parens (pretty x)
    pretty (LitNumber t i) = "#" <> pretty t <> "#" <> pretty i

instance Pretty AltCon where
    pretty (AltDataCon dc) = pretty dc
    pretty (AltLit l) = pretty l
    pretty AltDefault = text "DEFAULT"

instance Pretty Binder where
    pretty = pprBinder

pprExpr :: Expr -> Doc
pprExpr = pprExpr' False

pprAlt :: Alt -> Doc
pprAlt (Alt con bndrs rhs) = hang' (hsep (pretty con : map (pprBinder) bndrs) <+> smallRArrow) 2 (pprExpr' False rhs)

pprArg :: Arg -> Doc
pprArg = \case
  StgVarArg o -> pprBinder o
  StgLitArg l -> pretty l

instance Pretty Safety where
  pretty = text . show

instance Pretty CCallConv where
  pretty = text . show

instance Pretty CCallTarget where
  pretty = text . show

instance Pretty ForeignCall where
  pretty ForeignCall{..} = braces $ hsep [pretty foreignCSafety, pretty foreignCConv, pretty foreignCTarget]

pprOp :: StgOp -> Doc
pprOp = \case
  StgPrimOp op    -> text "_stg_prim_" <> pretty op
  StgPrimCallOp _ -> text "_stg_prim_call"
  StgFCallOp f    -> text "_stg_foreign_call" <+> pretty f

pprExpr' :: Bool -> Expr -> Doc
pprExpr' parens exp = case exp of
  StgLit l            -> pretty l
  StgCase x b alts    -> maybeParens parens
                         $ sep [ sep [ "case" <+> pprExpr' False x
                                     , "of" <+> pprBinder b <+> "{" ]
                               , indent 2 $ vcat $ map (pprAlt) alts
                               , "}"
                               ]
  StgApp f args         -> maybeParens parens $ hang' (pprBinder f) 2 (sep $ map (pprArg) args)
  StgOpApp op args ty r -> maybeParens parens $ hang' (pprOp op <+> braces (pretty ty)) 2 (sep $ map (pprArg) args) <+> ppRep r
  StgConApp dc args t r -> maybeParens parens $ hang' (pretty dc <+> comment (pretty t)) 2 (sep $ map (pprArg) args) <+> (hsep $ map ppRep r)
  StgLam b x            -> maybeParens parens $ hang' ("\\" <+> sep (map (pprBinder) b) <+> smallRArrow) 2 (pprExpr' False x)
  StgLet b e            -> maybeParens parens $ "let" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)
  StgLetNoEscape b e    -> maybeParens parens $ "lettail" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)


instance Pretty Expr where
  pretty = pprExpr


pprRhs :: Rhs -> Doc
pprRhs = \case
  StgRhsClosure vs u bs e -> text "closure" <+> parens (sep $ text "F:" : map pprBinder vs) <+> parens (sep $ text "B:" : map pprBinder bs) <+>
                              braces (line <> pprExpr e)
  StgRhsCon d vs          -> pretty d <+> (sep $ map (pprArg) vs)

pprBinding :: Binding -> Doc
pprBinding = \case
  StgNonRec b r  -> pprTopBind (b,r)
  StgRec bs      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  where
    pprTopBind (b,rhs) =
      hang' (pprBinder b <+> equals) 2 (pprRhs rhs)
      <> line

pprTopBinding :: TopBinding -> Doc
pprTopBinding = \case
  StgTopLifted (StgNonRec b r)  -> pprTopBind (b,r)
  StgTopLifted (StgRec bs)      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  StgTopStringLit b s           -> pprTopBind' (text . show) (b,s)
  where
    pprTopBind = pprTopBind' pprRhs
    pprTopBind' f (b,rhs) =
      hang' (pprBinder b <+> equals) 2 (f rhs)
      <> line

instance Pretty TopBinding where
  pretty = pprTopBinding

{-
  = Module
    { modulePhase       :: T_Text
    , moduleName        :: ModuleName
    , moduleDependency  :: [ModuleName]
    , moduleExternals   :: [(ModuleName, [bndr])]
    , moduleDataCons    :: [(ModuleName, [bndr])]
    , moduleExported    :: [(ModuleName, [BinderId])]
    , moduleTopBindings :: [TopBinding' bndr occ]
    }
-}
pprModule :: Module -> Doc
pprModule m =
  comment (pretty $ modulePhase m)
  <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line
  <$$> vsep [text "using" <+> pretty n | n <- moduleDependency m] <> line
  <$$> text "externals" <$$> vsep [indent 2 $ vsep (map pretty bl) | (n, bl) <- moduleExternals m] <> line
  <$$> text "data" <$$> vsep [indent 2 $ vsep (map pretty bl) | (n, bl) <- moduleDataCons m] <> line
  <$$> vsep (map (pprTopBinding) (moduleTopBindings m))

instance Pretty Module where
  pretty = pprModule
