{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Lambda.Pretty
  ( printLambda
  , prettyExternals
  ) where

import qualified Data.Vector as V
import Data.List (groupBy)
import qualified Data.Set as Set

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen as Leijen

import Lambda.Syntax
import qualified Lambda.Syntax2 as L2
import Grin.Pretty ()

printLambda :: Exp -> IO ()
printLambda exp = putDoc (pretty exp) >> putStrLn ""


keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

prettyBind (name, exp) = pretty name <+> text "=" <+> pretty exp
comment d = text "{-" <+> d <+> text "-}"

instance Pretty Exp where
  pretty prg = cata folder prg where
    extNames      = case prg of
                      Program exts _  -> Set.fromList $ map eName exts
                      _               -> Set.empty
    isPrimName n  = Set.member n extNames

    folder = \case
      ProgramF exts defs  -> vcat (prettyExternals exts : map pretty defs)
      DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : text "$" : map pretty args)
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
      ClosureF vars args exp -> nest 2 (keyword "\\" <> brackets (hsep (map pretty vars)) <+> hsep (map pretty args) <+> text "->" Leijen.<$> pretty exp)

instance Pretty L2.Exp where
  pretty prg = cata folder prg where
    extNames      = case prg of
                      L2.Program exts _  -> Set.fromList $ map L2.eName exts
                      _               -> Set.empty
    isPrimName n  = Set.member n extNames

    folder = \case
      L2.ProgramF exts defs  -> vcat (prettyExternals2 exts : map pretty defs)
      L2.DefF name args exp  -> nest 2 (hsep (pretty name : map pretty args) <+> text "=" <$$> pretty exp) <> line
      -- Exp
      L2.AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : text "$" : map pretty args)
      L2.CaseF atom alts     -> nest 2 (keyword "case" <+> pretty atom <+> keyword "of" <$$> vsep (map pretty alts))
      L2.LetF binds exp      -> nest 2 (keyword "let"    <$$> vsep (map prettyBind binds)) <$$> pretty exp
      L2.LetRecF binds exp   -> nest 2 (keyword "letrec" <$$> vsep (map prettyBind binds)) <$$> pretty exp
      L2.LetSF binds exp     -> nest 2 (keyword "letS"   <$$> vsep (map prettyBind binds)) <$$> pretty exp
      L2.ConF tag args       -> brackets $ hsep (pretty tag : map pretty args)
      -- Atom
      L2.VarF name           -> pretty name
      L2.LitF lit            -> pretty lit
      -- Alt
      L2.AltF name cpat exp  -> nest 2 (pretty cpat <+> text "@" <+> pretty name <+> text "->" <$$> pretty exp)
      -- Extra
      L2.ClosureF vars args exp -> nest 2 (keyword "\\" <> brackets (hsep (map pretty vars)) <+> hsep (map pretty args) <+> text "->" Leijen.<$> pretty exp)

instance Pretty Lit where
  pretty = \case
    LInt64 a  -> text "#T_Int64"    <+> integer (fromIntegral a)
    LWord64 a -> text "#T_Word64"   <+> integer (fromIntegral a)
    LFloat a  -> text "#T_Float"    <+> text (show a)
    LDouble a -> text "#T_Double"   <+> text (show a)
    LBool a   -> text "#T_Bool"     <+> text (show a)
    LChar a   -> text "#T_Char"     <+> text (show a)
    LString a -> text "#T_String"   <+> text (show a)
    LLabelAddr a -> text "#T_Addr"  <+> text (show a)
    LNullAddr -> text "#T_Addr"     <+> text "NullAddr"
    LToken a  -> text "#T_Token"    <+> text (show a)
--    LDummy a  -> red $ text "%" <> pretty a
    LError a  -> red $ text "#!" <> text (show a)

instance Pretty Pat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "_"

prettyExternals :: [External] -> Doc
prettyExternals exts = vcat (map prettyExtGroup $ groupBy (\a b -> (eKind a, eEffectful a) == (eKind b, eEffectful b)) exts) where
  maxWidth = 80
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _)
    | maxLen <- maximum [length . show . pretty $ eName e | e <- l]
    , width  <- min maxLen maxWidth
    = (prettyEKind (eKind a) <+> (if eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
        (vsep [prettyFunctionTy width eName eType | External{..} <- l])
      ) <> line
  prettyEKind = keyword . \case
    PrimOp  -> "primop"
    FFI     -> "ffi"

prettyExternals2 :: [L2.External] -> Doc
prettyExternals2 exts = vcat (map prettyExtGroup $ groupBy (\a b -> (L2.eKind a, L2.eEffectful a) == (L2.eKind b, L2.eEffectful b)) exts) where
  maxWidth = 80
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _)
    | maxLen <- maximum [length . show . pretty $ L2.eName e | e <- l]
    , width  <- min maxLen maxWidth
    = (prettyEKind (L2.eKind a) <+> (if L2.eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
        (vsep [prettyFunction width eName eRetType eArgsType | L2.External{..} <- l])
      ) <> line
  prettyEKind = keyword . \case
    PrimOp  -> "primop"
    FFI     -> "ffi"

prettyFunctionTy :: Int -> Name -> Ty -> Doc
prettyFunctionTy width name ty = fill width (pretty name) <> align (text " :: " <> pretty ty) where

prettyFunction :: Pretty a => Int -> Name -> a -> [a] -> Doc
prettyFunction width name ret args = fill width (pretty name) <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))

instance Pretty Ty where
  pretty = \case
    TyCon name tys      -> braces . hsep $ (green $ pretty name) : map (\a -> (if isTyArr a then parens else id) $ pretty a) tys
    TyVar name          -> text "%" <> cyan (pretty name)
    TySimple simpleType -> pretty simpleType
    TyArr a b           -> (if isTyArr a then parens else id) (pretty a) <+> text "->" <+> pretty b where

isTyArr :: Ty -> Bool
isTyArr = \case
  TyArr{} -> True
  _       -> False

instance Pretty L2.Ty where
  pretty = \case
    L2.TyCon varName name tys       -> braces (hsep ((green $ pretty name) : map pretty tys)) <+> text "@" <+> pretty varName
    L2.TyVar name                   -> text "%" <> cyan (pretty name)
    L2.TySimple varName simpleType  -> parens (pretty simpleType) <+> text "@" <+> pretty varName
    L2.TyFun name retTy argsTy      -> parens (pretty name <> encloseSep (text " : ") empty (text " -> ") (map pretty $ argsTy ++ [retTy]))

instance Pretty SimpleType where
  pretty = \case
    T_Token t -> red $ text "#" <> text (show t)
    ty        -> red $ text $ show ty
