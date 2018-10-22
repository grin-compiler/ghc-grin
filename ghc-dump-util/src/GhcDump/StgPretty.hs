{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcDump.StgPretty
  ( Pretty(..)
  , module GhcDump.StgPretty
  ) where

import GhcDump.Pretty
  ( PrettyOpts(..)
  , defaultPrettyOpts
  , comment
  , pprTypeSig
  , pprIdInfo
  , pprBinder
  , hang'
  , maybeParens
  , smallRArrow
  )
import GhcDump_Ast (BinderId(..), Binder(..), Unique(..), binderId, SBinder, binderIdInfo, binderIdDetails)
import GhcDump_StgAst
import GhcDump.Util

import Data.Ratio
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen

{-
data ForeignCall
data PrimOp
data PrimCall = PrimCall -- T_Text T_Text
data UpdateFlag = ReEntrant | Updatable | SingleEntry
data StgOp
-}


pprExpr :: PrettyOpts -> Expr -> Doc
pprExpr opts = pprExpr' opts False

pprAlt :: PrettyOpts -> Alt -> Doc
pprAlt opts (Alt con bndrs rhs) = hang' (hsep (pretty con : map (pprBinder opts) bndrs) <+> smallRArrow) 2 (pprExpr' opts False rhs)

pprArg :: PrettyOpts -> Arg -> Doc
pprArg opts = \case
  StgVarArg o -> pprBinder opts o
  StgLitArg l -> pretty l

pprOp :: PrettyOpts -> StgOp -> Doc
pprOp opt _ = text "op" -- TODO

pprCon :: DataCon -> Doc
pprCon (DataCon dc) = text $ BS.unpack dc

pprExpr' :: PrettyOpts -> Bool -> Expr -> Doc
pprExpr' opts parens exp = case exp of
  StgLit l            -> pretty l
  StgCase x b alts    -> maybeParens parens
                         $ sep [ sep [ "case" <+> pprExpr' opts False x
                                     , "of" <+> pprBinder opts b <+> "{" ]
                               , indent 2 $ vcat $ map (pprAlt opts) alts
                               , "}"
                               ]
  StgApp f args       -> maybeParens parens $ hang' (pprBinder opts f) 2 (sep $ map (pprArg opts) args)
  StgOpApp op args    -> maybeParens parens $ hang' (pprOp opts op) 2 (sep $ map (pprArg opts) args)
  StgConApp dc args   -> maybeParens parens $ hang' (pprCon dc) 2 (sep $ map (pprArg opts) args)
  StgLam b x          -> maybeParens parens $ hang' ("λ" <+> sep (map (pprBinder opts) b) <+> smallRArrow) 2 (pprExpr' opts False x)
  StgLet b e          -> maybeParens parens $ "let" <+> (align $ pprBinding opts b) <$$> "in" <+> align (pprExpr' opts False e)
  StgLetNoEscape b e  -> maybeParens parens $ "lettail" <+> (align $ pprBinding opts b) <$$> "in" <+> align (pprExpr' opts False e)


instance Pretty Expr where
  pretty = pprExpr defaultPrettyOpts


pprRhs :: PrettyOpts -> Rhs -> Doc
pprRhs opts = \case
  StgRhsClosure b vs u bs e -> pprExpr opts e -- TODO: StgRhsClosure b ([]) u ([]) (reconExpr bm e) -- TODO: maybe add binders to the binder map
  StgRhsCon d vs            -> pprCon d <+> (sep $ map (pprArg opts) vs)

pprBinding :: PrettyOpts -> Binding -> Doc
pprBinding opts = \case
  StgNonRec b r  -> pprTopBind (b,r)
  StgRec bs      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  where
    pprTopBind (b@(Bndr b'),rhs) =
      pprTypeSig opts b
      <$$> pprIdInfo opts (binderIdInfo b') (binderIdDetails b')
      <$$> hang' (pprBinder opts b <+> equals) 2 (pprRhs opts rhs)
      <> line

pprTopBinding :: PrettyOpts -> TopBinding -> Doc
pprTopBinding opts = \case
  StgTopLifted (StgNonRec b r)  -> pprTopBind (b,r)
  StgTopLifted (StgRec bs)      -> "rec" <+> braces (line <> vsep (map pprTopBind bs))
  StgTopStringLit b s           -> pprTopBind' (\_ -> text . show) (b,s)
  where
    pprTopBind = pprTopBind' pprRhs
    pprTopBind' f (b@(Bndr b'),rhs) =
      pprTypeSig opts b
      <$$> pprIdInfo opts (binderIdInfo b') (binderIdDetails b')
      <$$> hang' (pprBinder opts b <+> equals) 2 (f opts rhs)
      <> line

instance Pretty TopBinding where
  pretty = pprTopBinding defaultPrettyOpts

pprModule :: PrettyOpts -> Module -> Doc
pprModule opts m =
  comment (pretty $ modulePhase m)
  <$$> text "module" <+> pretty (moduleName m) <+> "where" <> line
  <$$> vsep (map (pprTopBinding opts) (moduleTopBindings m))

instance Pretty Module where
  pretty = pprModule defaultPrettyOpts
