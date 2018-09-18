{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Parse (parseCode) where

import Control.Monad
import Prelude hiding (id, last, succ)

-- Note: We do not need to import Hoopl to build an AST.
import Ast
import Expr
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-- I'm stealing this parser almost directly from Daan Leijen's Parsec guide.
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef {reservedOpNames = ["+", "-", "*", "/", "=", "<"]})

-- Common lexers:
lexeme, parens, braces :: CharParser () a -> CharParser () a
lexeme = P.lexeme lexer
parens = P.parens lexer
braces = P.braces lexer

commaSep :: CharParser () a -> CharParser () [a]
commaSep   = P.commaSep   lexer

reserved :: String -> CharParser () ()
reserved = P.reserved lexer

ign :: GenParser Char st a -> GenParser Char st ()
ign p = p >> return ()

char' :: Char -> GenParser Char st ()
char' c = ign $ char c

identifier :: CharParser () String
identifier = P.identifier lexer

natural :: CharParser () Integer
natural = P.natural    lexer

reservedOp :: String -> CharParser () ()
reservedOp = P.reservedOp lexer

whitespace :: CharParser () ()
whitespace = P.whiteSpace lexer

brackets :: CharParser () a -> CharParser () a
brackets = P.brackets lexer

-- Expressions:
expr :: Parser Expr
expr = buildExpressionParser table factor
    <?> "Expression"
  where
    table = [[op "*"  (Binop Mul) AssocLeft, op "/"  (Binop Div) AssocLeft],
             [op "+"  (Binop Add) AssocLeft, op "-"  (Binop Sub) AssocLeft],
             [op "="  (Binop Eq)  AssocLeft, op "/=" (Binop Ne)  AssocLeft,
              op ">"  (Binop Gt)  AssocLeft, op "<"  (Binop Lt)  AssocLeft,
              op ">=" (Binop Gte) AssocLeft, op "<=" (Binop Lte) AssocLeft]]
    op o f assoc = Infix (do {reservedOp o; return f} <?> "operator") assoc
    factor =   parens expr
           <|> lit
           <|> load
           <|> fetchVar
           <?> "simple Expression"

bool :: Parser Bool
bool =  (try $ lexeme (string "True")  >> return True)
    <|> (try $ lexeme (string "False") >> return False)

lit :: Parser Expr
lit =  (natural >>= (return . Lit . Int))
   <|> (bool    >>= (return . Lit . Bool))
   <|> (bool    >>= (return . Lit . Bool))
   <?> "lit"

loc :: Char -> Parser x -> Parser x
loc s addr = try (lexeme (char' s >> brackets addr))
          <?> "loc"

var :: Parser String
var  = identifier
    <?> "var"

mem :: Parser Expr -- address
mem  =  loc 'm' expr
    <?> "mem"

fetchVar, load :: Parser Expr
fetchVar = var >>= return . Var
load     = mem >>= return . Load


labl :: Parser Lbl
labl = lexeme (do { id <- identifier
                  ; char' ':'
                  ; return id
                  })
  <?> "label"

mid :: Parser Insn
mid =   try asst
    <|> store
    <?> "assignment or store"

asst :: Parser Insn
asst = do { v <- lexeme var
          ; lexeme (char' '=')
          ; e <- expr
          ; return $ Assign v e
          }
    <?> "asst"

store :: Parser Insn
store = do { addr <- lexeme mem
           ; lexeme (char' '=')
           ; e <- expr
           ; return $ Store addr e
           }
     <?> "store"

control :: Parser Control
control =  branch
       <|> cond
       <|> call
       <|> ret
       <?> "control-transfer"


goto :: Parser Lbl
goto = do { lexeme (reserved "goto")
          ; identifier
          }
    <?> "goto"

branch :: Parser Control
branch =
    do { l <- goto
       ; return $ Branch l
       }
 <?> "branch"

cond, call, ret :: Parser Control
cond =
  do { lexeme (reserved "if")
     ; cnd <- expr
     ; lexeme (reserved "then")
     ; thn <- goto
     ; lexeme (reserved "else")
     ; els <- goto
     ; return $ Cond cnd thn els
     }
 <?> "cond"

call =
  do { results <- tuple var
     ; lexeme (char' '=')
     ; f <- identifier
     ; params <- tuple expr
     ; succ   <- goto
     ; return $ Call results f params succ
     }
 <?> "call"

ret =
  do { lexeme (reserved "ret")
     ; results <- tuple expr
     ; return $ Return results
     }
 <?> "ret"

block :: Parser Block
block =
  do { f   <- lexeme labl
     ; ms  <- many $ try mid
     ; l   <- lexeme control
     ; return $ Block { first = f, mids = ms, last = l }
     }
 <?> "Expected basic block; maybe you forgot a label following a control-transfer?"

tuple :: Parser a -> Parser [a]
tuple = parens . commaSep

proc :: Parser Proc
proc = do { whitespace
          ; f      <- identifier
          ; params <- tuple  var
          ; bdy    <- braces $ do { b <- block
                                  ; bs <- many block
                                  ; return (b : bs)
                                  } -- procedure must have at least one block
          ; return $ Proc { name = f, args = params, body = bdy }
          }
    <?> "proc"

parseCode :: String -> String -> Either ParseError [Proc]
parseCode file inp = parse (many proc) file inp
