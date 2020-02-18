{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings #-}

module Lambda.Parse
  ( parseLambda
  , parseProg
  ) where

import Data.Ratio ((%))
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

import qualified Data.Text.IO as T
import qualified Text.Megaparsec as M

main = do
  let fname = "basic00.2.lambda"
  content <- T.readFile fname
  case parseLambda fname content of
    Right{} -> putStrLn "OK"
    Left err -> do
      error . M.parseErrorPretty' content $ err

-- indetifier rules for parser and pretty printer
allowedSpecial :: String
allowedSpecial = "._':!@-"

allowedInitial :: String
allowedInitial = "._" ++ ['a'..'z'] ++ ['A'..'Z']

keywords = Set.fromList ["case","of","let","letrec","letS", "primop", "ffi", "effectful", "_"]

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
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

kw w = lexeme $ string w

op w = L.symbol sc' w

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
var = lexeme (quotedVar <|> simpleVar) >>= \x -> case Set.member (unpackName x) keywords of
  True -> fail $ "invalid identifier name, it can not be reserved word: " ++ unpackName x
  False -> return x

----
primRep :: Parser PrimRep
primRep = choice
  [ VoidRep     <$ kw "VoidRep"
  , LiftedRep   <$ kw "LiftedRep"
  , UnliftedRep <$ kw "UnliftedRep"
  , Int64Rep    <$ kw "Int64Rep"
  , Word64Rep   <$ kw "Word64Rep"
  , AddrRep     <$ kw "AddrRep"
  , FloatRep    <$ kw "FloatRep"
  , DoubleRep   <$ kw "DoubleRep"
  ]

repType :: Parser RepType
repType = choice
  [ UnboxedTuple <$> braces (many primRep)
  , SingleValue  <$> primRep
  , PolymorphicRep <$ kw "PolymorphicRep"
  ]

binder :: Parser (Name, RepType)
binder = (,) <$> var <* op ":" <*> repType

binderArg :: Parser (Name, RepType)
binderArg = parens binder

integer = lexeme L.decimal
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

rational = (%) <$> signedInteger <* op "%" <*> integer

-- lambda syntax

def :: Parser Def
def = do
  L.indentGuard sc EQ pos1
  L.indentBlock sc $ do
    name <- var
    args <- many binderArg
    op "="
    pure $ L.IndentSome Nothing (\l -> Def name args <$> mkBind l) bind

data BindKind
  = Recursive
  | Strict
  | Lazy
  | ResultVar Name

bind :: Parser (BindKind, [(Name, RepType, SimpleExp)])
bind = do
  L.indentBlock sc $ do
    kind <- choice
      [ Recursive <$  kw "letrec"
      , Strict    <$  kw "letS"
      , Lazy      <$  kw "let"
      , ResultVar <$> var
      ]
    pure $ case kind of
      ResultVar{} -> L.IndentNone (kind, [])
      _ -> L.IndentSome Nothing (pure . (kind,)) varBind

mkBind :: [(BindKind, [(Name, RepType, SimpleExp)])] -> Parser BindChain
mkBind [(ResultVar v, [])]    = pure $ Var v
mkBind [_]                    = fail "bind chains must end with a result variable"
mkBind ((Recursive, l) : xs)  = LetRec l <$> mkBind xs
mkBind ((Strict, l) : xs)     = LetS l <$> mkBind xs
mkBind ((Lazy, l) : xs)       = Let l <$> mkBind xs
mkBind _                      = fail "invalid bind chain"

data ExpKind
  = KClosure
  | KCase
  | KOther

expKind :: Parser ExpKind
expKind = lookAhead $ do
  var
  op "="
  choice
    [ KClosure <$ op "\\"
    , KCase <$ kw "case"
    , pure KOther
    ]

varBind :: Parser (Name, RepType, SimpleExp)
varBind = do
  expKind >>= \case
    KClosure  -> closure
    KCase     -> caseP
    KOther    -> simpleExp

simpleExp :: Parser (Name, RepType, SimpleExp)
simpleExp = do
  (n, t) <- binder
  op "="
  e <- choice
    [ brackets (Con <$> var <*> many var)
    , Lit <$> literal
    , do
        f <- var
        choice
          [ App f <$ op "$" <*> many var
          , pure $ Var f
          ]
    ]
  pure (n, t, e)

caseP :: Parser (Name, RepType, SimpleExp)
caseP = do
  L.indentBlock sc $ do
    (n, t) <- binder
    op "="
    kw "case"
    scrut <- var
    kw "of"
    pure $ L.IndentSome Nothing (\alts -> pure (n, t, Case scrut alts)) alternative

closure :: Parser (Name, RepType, SimpleExp)
closure = do
  L.indentBlock sc $ do
    (n, t) <- binder
    op "="
    op "\\"
    captured <- brackets (many var)
    params <- many binderArg
    op "->"
    pure $ L.IndentSome Nothing (\l -> (n, t,) . Closure captured params <$> mkBind l) bind

alternative :: Parser Alt
alternative = do
  L.indentBlock sc $ do
    pat <- altPat
    op "@"
    altName <- var
    op "->"
    pure $ L.IndentSome Nothing (\l -> Alt altName pat <$> mkBind l) bind

altPat :: Parser Pat
altPat = parens (NodePat <$> var <*> many var) <|>
         LitPat <$> literal <|>
         DefaultPat <$ kw "_"

bstrLiteral :: Parser BS8.ByteString
bstrLiteral = lexeme (BS8.pack <$> (char '"' >> manyTill L.charLiteral (char '"')))

literal :: Parser Lit
literal = do
  char '#'
  choice
    [ LBool                   <$ kw "T_Bool"    <*> (True <$ kw "True" <|> False <$ kw "False")
    , LFloat . realToFrac     <$ kw "T_Float"   <*> rational
    , LWord64 . fromIntegral  <$ kw "T_Word64"  <*> lexeme L.decimal
    , LInt64 . fromIntegral   <$ kw "T_Int64"   <*> signedInteger
    , LChar                   <$ kw "T_Char"    <*  char '\'' <*> L.charLiteral <* char '\'' <* sc
    , LString                 <$ kw "T_String"  <*> bstrLiteral
    , LDouble . realToFrac    <$ kw "T_Double"  <*> rational
    , LToken                  <$ kw "T_Token"   <*> bstrLiteral
    , LError                  <$ char '!'       <*> bstrLiteral
    , kw "T_Addr" >> choice
        [ LNullAddr <$ kw "NullAddr"
        , LCodeAddr <$ kw "CodeAddr" <*> bstrLiteral <*> optional (lexeme L.decimal)
        , LDataAddr <$ kw "DataAddr" <*> bstrLiteral
        ]
    ]

-- con groups

conSpec :: Parser ConSpec
conSpec = ConSpec <$> var <*> many primRep

conGroup :: Parser ConGroup
conGroup = do
  L.indentGuard sc GT pos1
  L.indentBlock sc $ do
    kw "data"
    name <- var
    pure $ L.IndentSome Nothing (\cons -> pure $ ConGroup name cons) conSpec

conGroups :: Parser [ConGroup]
conGroups = do
  L.indentGuard sc EQ pos1
  L.indentBlock sc $ do
    kw "constructors"
    pure $ L.IndentSome Nothing pure conGroup

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
  L.indentBlock sc $ do
    kind <-
      PrimOp <$ kw "primop" <|>
      FFI <$ kw "ffi"
    eff <- const False <$> kw "pure" <|> const True <$> kw "effectful"
    pure $ L.IndentSome Nothing pure (external kind eff)

external :: ExternalKind -> Bool -> Parser External
external kind eff = do
  name <- var
  op "::"
  (retTy, argsTy) <- tyArrP
  sc
  pure External
    { eName       = name
    , eRetType    = retTy
    , eArgsType   = argsTy
    , eEffectful  = eff
    , eKind       = kind
    }

tyArrP :: Parser (Ty, [Ty])
tyArrP = do
  t <- tyP `sepBy1` op "->"
  let (retTy : argsTy) = reverse t
  return (retTy, reverse argsTy)

tyP :: Parser Ty
tyP = choice
  [ TyVar <$ C.char '%' <*> var
  , do
      (tag, args) <- braces ((,) <$> var <*> many tyP)
      op "@"
      nodeName <- var
      pure $ TyCon nodeName tag args
  , try $ do
      ty <- parens simpleType
      op "@"
      nodeName <- var
      pure $ TySimple nodeName ty
  , parens $ do
      nodeName <- var
      op ":"
      (retTy, argsTy) <- tyArrP
      pure $ TyFun nodeName retTy argsTy
  ]

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
lambdaModule = Program <$> (concat <$> many externalBlock) <*> option [] conGroups <*> option [] staticDataBlock <*> many def <* sc <* eof

parseLambda :: String -> Text -> Either (ParseError Char Void) Program
parseLambda filename content = runParser lambdaModule filename content

parseProg :: Text -> Exp
parseProg src = either (error . parseErrorPretty' src) id . parseLambda "" $ src
