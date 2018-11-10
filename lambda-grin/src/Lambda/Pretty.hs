{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Lambda.Pretty
  ( printLambda
  ) where

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

instance Pretty Exp where
  pretty = cata folder where
    folder = \case
      ProgramF defs       -> vcat (map pretty defs)
      DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : map pretty args)
      CaseF atom alts     -> keyword "case" <+> pretty atom <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      LetF binds exp      -> keyword "let"    <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetRecF binds exp   -> keyword "letrec" <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetSF binds exp     -> keyword "letS"   <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      ConF tag args       -> brackets $ hsep (pretty tag : map pretty args)
      -- Atom
      VarF name           -> pretty name
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
    LFloat a  -> float a
    LBool a   -> text "#" <> text (show a)
    LChar a   -> text "#" <> text (show a)
    LString a -> text "#" <> text (show a)
    LDummy a  -> red $ text "%" <> pretty a
    LError a  -> red $ text "!" <> pretty a

instance Pretty Pat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "_"
