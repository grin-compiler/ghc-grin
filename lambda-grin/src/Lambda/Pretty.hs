{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Lambda.Pretty
  ( printLambda
  , prettyExternals
  ) where

import qualified Data.Vector as V
import Data.List (groupBy)

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Lambda.Syntax
import Grin.Grin (isPrimName)
import Grin.Pretty ()

printLambda :: Exp -> IO ()
printLambda exp = putDoc (pretty exp) >> putStrLn ""


keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

prettyBind (name, exp) = pretty name <+> text "=" <+> pretty exp
comment d = text "{-" <+> d <+> text "-}"

instance Pretty Exp where
  pretty = cata folder where
    folder = \case
      ProgramF exts defs  -> vcat (prettyExternals exts : map pretty defs)
      DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : text "@" : map pretty args)
      CaseF atom alts     -> keyword "case" <+> pretty atom <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      LetF binds exp      -> keyword "let"    <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetRecF binds exp   -> keyword "letrec" <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetSF binds exp     -> keyword "letS"   <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      ConF tag args       -> brackets $ hsep (pretty tag : map pretty args)
      -- Atom
      VarF isPtr name     -> pretty name <> if isPtr then mempty else comment (text "value")
      LitF lit            -> pretty lit
      -- Alt
      AltF cpat exp       -> pretty cpat <+> text "->" <$$> indent 4 (pretty exp)
      -- Extra
      AppExpF exp args    -> hsep $ parens exp : map pretty args
      LamF name exp       -> keyword "\\" <> hsep (map pretty name) <+> text "->" <+> align (pretty exp)

instance Pretty Lit where
  pretty = \case
    LInt64 a  -> integer $ fromIntegral a
    LWord64 a -> integer (fromIntegral a) <> text "u"
    LFloat a  -> text "#" <> text (show a)
    LDouble a -> text "#" <> text (show a) <> text "d"
    LBool a   -> text "#" <> text (show a)
    LChar a   -> text "#" <> text (show a)
    LString a -> text "#" <> text (show a)
    LLabelAddr a -> text "#@" <> text (show a)
    LNullAddr -> text "#NullAddr"
--    LDummy a  -> red $ text "%" <> pretty a
    LError a  -> red $ text "!" <> text (show a)

instance Pretty Pat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "_"

prettyExternals :: [External] -> Doc
prettyExternals exts = vcat (map prettyExtGroup $ groupBy (\a b -> eEffectful a == eEffectful b) exts) where
  maxWidth = 80
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _)
    | maxLen <- maximum [length . show . pretty $ eName e | e <- l]
    , width  <- min maxLen maxWidth
    = (keyword "primop" <+> (if eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
        (vsep [prettyFunction width eName eRetType eArgsType | External{..} <- l])
      ) <> line

prettyFunction :: Pretty a => Int -> Name -> a -> [a] -> Doc
prettyFunction width name ret args = fill width (pretty name) <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))

instance Pretty Ty where
  pretty = \case
    TyCon name tys      -> braces . hsep $ (green $ pretty name) : map pretty tys
    TyVar name          -> text "%" <> cyan (pretty name)
    TySimple simpleType -> pretty simpleType

instance Pretty SimpleType where
  pretty = \case
    ty -> red $ text $ show ty
