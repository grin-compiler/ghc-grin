{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stg.Pretty
  ( Pretty(..)
  , module Stg.Pretty
  ) where

import Stg.Syntax

import Data.Ratio
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen

{-
data ForeignCall
data PrimCall = PrimCall -- Name Name
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

ppType :: Type -> Doc
ppType = red . text . show

pprBinder :: Binder -> Doc
pprBinder b = parens $ (exported . pretty . binderUniqueName $ b) <+> text "::" <+> ppType (binderType b) where
  BinderId u  = binderId b
  exported    = if binderIsExported b then green else id

instance Pretty Type where
    pretty = ppType

instance Pretty Name where
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
    pretty (LitChar x) = "'" <> char x <> "'#"
    pretty (LitString x) = "\"" <> text (BS.unpack x) <> "\"#"
    pretty LitNullAddr = "nullAddr#"
    pretty (LitFloat x) = "FLOAT" <> parens (pprRational x)
    pretty (LitDouble x) = "DOUBLE" <> parens (pprRational x)
    pretty (LitLabel x s) = "LABEL"<> parens (pretty x) <+> text (show s)
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
  StgApp f args ty s    -> maybeParens parens $ hang' (pprBinder f) 2 (sep $ map (pprArg) args) <+> text "::" <+> (pretty ty) <+> comment (pretty s)
  StgOpApp op args ty   -> maybeParens parens $ hang' (pprOp op) 2 (sep $ map (pprArg) args) <+> text "::" <+> (pretty ty)
  StgConApp dc args _t  -> maybeParens parens $ hang' (pretty dc) 2 (sep $ map (pprArg) args)
  StgLet b e            -> maybeParens parens $ "let" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)
  StgLetNoEscape b e    -> maybeParens parens $ "lettail" <+> (align $ pprBinding b) <$$> "in" <+> align (pprExpr' False e)


instance Pretty Expr where
  pretty = pprExpr


pprRhs :: Rhs -> Doc
pprRhs = \case
  StgRhsClosure u bs e -> text "closure" <+> parens (sep $ text "B:" : map pprBinder bs) <+> braces (line <> pprExpr e)
  StgRhsCon d vs -> pretty d <+> (sep $ map (pprArg) vs)

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

pprTyCon :: AlgTyCon -> Doc
pprTyCon AlgTyCon{..} = pretty modName <> text "." <> pretty tcName <$$> (indent 2 $ vsep (map pretty tcDataCons)) <> line where
  modName = sdcModule $ head tcDataCons -- NOTE: AlgTyCons must have at least one constructor

pprSDataCon :: SDataCon -> Doc
pprSDataCon SDataCon{..} = pretty sdcModule <> text "." <> pretty sdcName <+> text "::" <+> text (show sdcRep)

instance Pretty SDataCon where
    pretty = pprSDataCon

pprDataCon :: DataCon -> Doc
pprDataCon DataCon{..} = pretty dcModule <> text "." <> pretty dcName <+> text "::" <+> text (show dcRep)

instance Pretty DataCon where
    pretty = pprDataCon

instance Pretty AlgTyCon where
  pretty = pprTyCon

pprModule :: Module -> Doc
pprModule m =
  comment (pretty $ modulePhase m)
  <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line
  <$$> vsep [text "using" <+> pretty n | n <- moduleDependency m] <> line
  <$$> text "externals" <$$> vsep [indent 2 $ vsep (map pretty bl) | (n, bl) <- moduleExternalTopIds m] <> line
  <$$> text "type" <$$> vsep [indent 2 $ vsep (map pretty tl) | (n, tl) <- moduleAlgTyCons m] <> line
  <$$> vsep (map (pprTopBinding) (moduleTopBindings m))

instance Pretty Module where
  pretty = pprModule
