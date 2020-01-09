{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings #-}

module Lambda.Parse (parseLambda) where

import Data.String (fromString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import Data.Void
import Control.Applicative (empty)
import Control.Monad (void, mzero)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C
import qualified Data.Set as Set
import Lambda.Syntax

-- indetifier rules for parser and pretty printer
allowedSpecial :: String
allowedSpecial = "._':!@-"

allowedInitial :: String
allowedInitial = "._" ++ ['a'..'z'] ++ ['A'..'Z']

keywords = Set.fromList ["case","of","let","letrec","letS", "#True", "#False", "_", "static", "data"]

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment

sc' :: Parser ()
sc' = L.space (void $ oneOf (" \t" :: String)) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc'

symbol = L.symbol sc'
parens = between (symbol "(") (symbol ")")

kw w = lexeme $ string w

op w = L.symbol sc' w
{-
var :: Parser Name
var = try $ lexeme ((\c s -> packName $ c:s) <$> lowerChar <*> many (alphaNumChar <|> oneOf ("'_.:!@{}$-" :: String))) >>= \x -> case Set.member x keywords of
  True -> fail $ "keyword: " ++ unpackName x
  False -> pure x

con :: Parser Name
con = lexeme $ packName <$> some (alphaNumChar)
-}
----
escaped :: Parser Char
escaped = string "\\\"" >> pure '"'

quotedVar :: Parser Name
quotedVar = packName <$ char '"' <*> someTill (escaped <|> anyChar) (char '"')

escapedStringChar :: Parser Char
escapedStringChar =
  (string "\\\"" >> pure '"') <|>
  (string "\\\\" >> pure '\\') <|>
  (string "\\a" >> pure '\a') <|>
  (string "\\b" >> pure '\b') <|>
  (string "\\f" >> pure '\f') <|>
  (string "\\n" >> pure '\n') <|>
  (string "\\r" >> pure '\r') <|>
  (string "\\t" >> pure '\t') <|>
  (string "\\v" >> pure '\v')

quotedString :: Parser Text
quotedString = fromString <$> (char '"' *> manyTill (escapedStringChar <|> anyChar) (char '"'))

simpleVar :: Parser Name
simpleVar = (\c s -> packName $ c : s) <$> oneOf allowedInitial <*> many (alphaNumChar <|> oneOf allowedSpecial)

-- TODO: allow keywords in quotes
var :: Parser Name
var = try $ lexeme (quotedVar <|> simpleVar) >>= \x -> case Set.member (unpackName x) keywords of
  True -> fail $ "keyword: " ++ unpackName x
  False -> return x

con :: Parser Name
con = var
----
integer = lexeme L.decimal
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

braces = between (symbol "{") (symbol "}")

-- lambda syntax

def :: Parser Def
def = Def <$> try (L.indentGuard sc EQ pos1 *> primNameOrDefName) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

varBind :: Pos -> Parser (Name, Exp)
varBind i = (,) <$> try (L.indentGuard sc EQ i *> var) <* op "=" <*> (L.indentGuard sc GT i >>= expr)

expr :: Pos -> Parser Exp
expr i = L.indentGuard sc EQ i >>
  Case <$ kw "case"     <*> atom <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
  LetS <$ kw "letS"     <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  LetRec <$ kw "letrec" <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  Let  <$ kw "let"      <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  parens (Con <$> tag <*> many atom) <|>
  try (App <$> primNameOrDefName <*> some atom) <|>
  atom

atom :: Parser Atom
atom = Lit <$> literal <|>
       Var True <$> var -- TODO

primNameOrDefName :: Parser Name
primNameOrDefName = ("_"<>) <$ char '_' <*> var <|> var

alternative :: Pos -> Parser Alt
alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

altPat :: Parser Pat
altPat = parens (NodePat <$> tag <*> many var) <|>
         LitPat <$> literal <|>
         DefaultPat <$ kw "_"

tag :: Parser Name
tag = con

bstrLiteral :: Parser BS8.ByteString
bstrLiteral = BS8.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- FIXME: lit parser is not complatible with the Lit pretty printer
literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          LInt64 . fromIntegral <$> signedInteger <|>
          LBool <$> (True <$ kw "#True" <|> False <$ kw "#False") <|>
          LChar <$ char '#' <* char '\'' <* L.charLiteral <*> char '\'' <|>
          LString <$ char '#' <*> bstrLiteral

-- static data

staticData :: Parser StaticData
staticData = do
  name <- var
  op "="
  char '#'
  kw "T_String"
  bs <- bstrLiteral
  sc
  pure $ StaticData name $ StaticString bs

staticDataBlock :: Parser [StaticData]
staticDataBlock = do
  L.indentGuard sc EQ pos1
  L.indentBlock sc $ do
    kw "static"
    kw "data"
    pure $ L.IndentSome Nothing pure staticData

-- externals

externalBlock = do
  L.indentGuard sc EQ pos1
  kind <-
    PrimOp <$ kw "primop" <|>
    FFI <$ kw "ffi"
  eff <- const False <$> kw "pure" <|> const True <$> kw "effectful"
  i <- L.indentGuard sc GT pos1
  some $ try (external kind eff i)

external :: ExternalKind -> Bool -> Pos -> Parser External
external kind eff i = do
  L.indentGuard sc EQ i
  name <- var
  op "::"
  ty <- tyArrP
  pure External
    { eName       = name
    , eType       = ty
    , eEffectful  = eff
    , eKind       = kind
    }

tyArrP :: Parser Ty
tyArrP =
  try (TyArr <$> tyP <* op "->" <*> tyArrP) <|>
  tyP

tyP :: Parser Ty
tyP =
  parens tyArrP <|>
  TyVar <$ C.char '%' <*> var <|>
  braces (TyCon <$> var <*> many tyP) <|>
  TySimple <$> try simpleType

-- type syntax

simpleType :: Parser SimpleType
simpleType =
  T_Int64   <$ kw "T_Int64"   <|>
  T_Word64  <$ kw "T_Word64"  <|>
  T_Float   <$ kw "T_Float"   <|>
  T_Double  <$ kw "T_Double"  <|>
  T_Bool    <$ kw "T_Bool"    <|>
  T_Unit    <$ kw "T_Unit"    <|>
  T_String  <$ kw "T_String"  <|>
  T_Char    <$ kw "T_Char"    <|>
  T_Addr    <$ kw "T_Addr"    <|>
  T_Token   <$ char '#' <*> bstrLiteral

-- top-level API

lambdaModule :: Parser Program
lambdaModule = Program <$> (concat <$> many (try externalBlock)) <*> option [] staticDataBlock <*> many def <* sc <* eof

parseLambda :: String -> Text -> Either (ParseError Char Void) Program
parseLambda filename content = runParser lambdaModule filename content
