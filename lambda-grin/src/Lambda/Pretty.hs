{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Lambda.Pretty
  ( printLambda
  , prettyExternals
  , showName
  , showWidth
  , showWide
  , PP(..)
  ) where

import Data.Char
import qualified Data.Vector as V
import Data.List (groupBy)
import qualified Data.Set as Set

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen as Leijen

import Lambda.Syntax
import Lambda.Parse (allowedInitial, allowedSpecial)

-- Pretty Show instance wrapper ; i.e. useful for hspec tests
newtype PP a = PP a deriving Eq
instance Pretty a => Show (PP a ) where
  show (PP a) = showWide . plain . pretty $ a

printLambda :: Exp -> IO ()
printLambda exp = putDoc (pretty exp) >> putStrLn ""

keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

prettyBind (name, repType, exp) = pretty name <+> text ":" <+> pretty repType <+> text "=" <+> pretty exp
comment d = text "{-" <+> d <+> text "-}"

prettyBinder :: (Name, RepType) -> Doc
prettyBinder (name, repType) = pretty name <+> text ":" <+> pretty repType
prettyBinderArg = parens . prettyBinder

instance Pretty Exp where
  pretty prg = cata folder prg where
    extNames      = case prg of
                      Program{..} -> Set.fromList $ map eName pExternals
                      _           -> Set.empty
    isPrimName n  = Set.member n extNames

    folder = \case
      ProgramF{..} -> vcat ( prettyExternals pExternalsF
                           : prettyConGroups pConstructorsF
                           : prettyPublicNames pPublicNamesF
                           : prettyForeignExportedNames pForeignExportedNamesF
                           : prettyStaticData pStaticDataF
                           : map pretty pDefinitionsF
                           )
      DefF name args exp  -> nest 2 (hsep (pretty name : map prettyBinderArg args) <+> text "=" <$$> pretty exp) <> line
      -- Exp
      AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : text "$" : map pretty args)
      CaseF atom alts     -> nest 2 (keyword "case" <+> pretty atom <+> keyword "of" <$$> vsep (map pretty alts))
      LetF binds exp      -> nest 2 (keyword "let"    <$$> vsep (map prettyBind binds)) <$$> pretty exp
      LetRecF binds exp   -> nest 2 (keyword "letrec" <$$> vsep (map prettyBind binds)) <$$> pretty exp
      LetSF binds exp     -> nest 2 (keyword "letS"   <$$> vsep (map prettyBind binds)) <$$> pretty exp
      ConF tag args       -> brackets $ hsep (pretty tag : map pretty args)
      -- Atom
      VarF name           -> pretty name
      LitF lit            -> pretty lit
      -- Alt
      AltF name cpat exp  -> nest 2 (pretty cpat <+> text "@" <+> pretty name <+> text "->" <$$> pretty exp)
      -- Extra
      ClosureF vars args exp -> nest 2 (keyword "\\closure" <+> hsep (brackets (hsep (map pretty vars)) : map prettyBinderArg args ++ [text "->"]) Leijen.<$> pretty exp)

instance Pretty Lit where
  pretty = \case
    LInt64 a  -> text "#T_Int64"    <+> integer (fromIntegral a)
    LWord64 a -> text "#T_Word64"   <+> integer (fromIntegral a)
    LFloat a  -> text "#T_Float"    <+> text (show a)
    LDouble a -> text "#T_Double"   <+> text (show a)
    LBool a   -> text "#T_Bool"     <+> text (show a)
    LChar a   -> text "#T_Char"     <+> text (show a)
    LString a -> text "#T_String"   <+> text (show a)
    LDataAddr a   -> text "#T_Addr" <+> text "DataAddr" <+> text (show a)
    LCodeAddr a i -> text "#T_Addr" <+> text "CodeAddr" <+> text (show a) <+> pretty i
    LNullAddr -> text "#T_Addr"     <+> text "NullAddr"
    LToken a  -> text "#T_Token"    <+> text (show a)
    LError a  -> red $ text "#!" <> text (show a)

instance Pretty Pat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map pretty vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "_"

prettyForeignExportedNames :: [Name] -> Doc
prettyForeignExportedNames [] = mempty
prettyForeignExportedNames names = keyword "foreign exported names" <$$> vsep (map (indent 2 . pretty) names) <> line

prettyPublicNames :: [Name] -> Doc
prettyPublicNames [] = mempty
prettyPublicNames names = keyword "public names" <$$> vsep (map (indent 2 . pretty) names) <> line

prettyConGroups :: [ConGroup] -> Doc
prettyConGroups [] = mempty
prettyConGroups cons = keyword "constructors" <$$> line <> vsep (map prettyConGroup cons) <> line

prettyConGroup :: ConGroup -> Doc
prettyConGroup (ConGroup name cons) = indent 2 (keyword "data" <+> pretty name <$$> indent 2 (vsep $ map (prettyConSpec width) cons)) <> line where
  maxWidth  = 80
  maxLen    = maximum [length . show . pretty $ csName s | s <- cons]
  width     = min maxLen maxWidth

prettyConSpec :: Int -> ConSpec -> Doc
prettyConSpec _width (ConSpec name [])     = pretty name
prettyConSpec width (ConSpec name argsRep) = fill width (pretty name) <+> hsep (map pretty argsRep)

prettyStaticData :: [StaticData] -> Doc
prettyStaticData [] = mempty
prettyStaticData sdata = keyword "static" <+> keyword "data" <$$> indent 2 (vsep $ map (prettyStaticDataItem width) sdata) <> line where
  maxWidth  = 80
  maxLen    = maximum [length . show . pretty $ sName s | s <- sdata]
  width     = min maxLen maxWidth

prettyStaticDataItem :: Int -> StaticData -> Doc
prettyStaticDataItem width (StaticData name sv) = case sv of
  StaticString bs -> fill width (pretty name) <+> text "=" <+> text "#T_String" <+> text (show bs)

prettyExternals :: [External] -> Doc
prettyExternals exts = vcat (map prettyExtGroup $ groupBy (\a b -> (eKind a, eEffectful a) == (eKind b, eEffectful b)) exts) where
  maxWidth = 80
  prettyExtGroup [] = mempty
  prettyExtGroup l@(a : _)
    | maxLen <- maximum [length . show . pretty $ eName e | e <- l]
    , width  <- min maxLen maxWidth
    = (prettyEKind (eKind a) <+> (if eEffectful a then keyword "effectful" else keyword "pure") <$$> indent 2
        (vsep [prettyFunction width eName eRetType eArgsType | External{..} <- l])
      ) <> line
  prettyEKind = keyword . \case
    PrimOp  -> "primop"
    FFI     -> "ffi"

myEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
myEncloseSep l r _s [] = l <> r
myEncloseSep l r s (x : xs) = l <> x <> foldr (\i doc -> s <> i <> doc) empty xs <> r

prettyFunction :: Pretty a => Int -> Name -> a -> [a] -> Doc
prettyFunction width name ret args = fill width (pretty name) <> align (myEncloseSep (text " :: ") empty (text " -> ") (map pretty $ args ++ [ret]))

instance Pretty Ty where
  pretty = \case
    TyCon varName name tys       -> braces (hsep ((green $ pretty name) : map pretty tys)) <+> text "@" <+> pretty varName
    TyVar name                   -> text "%" <> cyan (pretty name)
    TySimple varName simpleType  -> parens (pretty simpleType) <+> text "@" <+> pretty varName
    TyFun name retTy argsTy      -> parens (pretty name <> (myEncloseSep (text " : ") empty (text " -> ") (map pretty $ argsTy ++ [retTy])))

instance Pretty SimpleType where
  pretty = \case
    T_Token t -> red $ text "#" <> text (show t)
    ty        -> red $ text $ show ty

instance Pretty PrimRep where
  pretty t = red $ text $ show t

instance Pretty RepType where
  pretty = \case
    SingleValue t   -> pretty t
    UnboxedTuple l  -> braces (hsep $ map pretty l)
    PolymorphicRep  -> red $ text "PolymorphicRep"

-- Name
showName :: Name -> String
showName n = case unpackName n of
  []    -> ""
  str@(c:s)
    | c `elem` allowedInitial && all (\a -> isAlphaNum a || elem a allowedSpecial) s -> str
    | otherwise -> '"' : go str
    where
      go [] = ['"']
      go ('"':xs) = '\\' : '"' : go xs
      go (a : xs) = a : go xs

instance Pretty Name where
  pretty = text . showName

showWide :: Doc -> String
showWide = showWidth 156

showWidth :: Int -> Doc -> String
showWidth w x = displayS (renderPretty 0.4 w x) ""
