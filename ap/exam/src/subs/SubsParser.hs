module SubsParser (
  parseString,
  parseFile
) where

import SubsAst as S
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Functor.Identity

{-data ParseError = AmbiguousParse
                | NoParse
  deriving (Show, Eq)-}

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- Reserved
reserved :: [String]
reserved = ["var", "true", "false", "undefined", "for", "of", "if"]

-- Ident implementation
ident :: Parser String
ident = do
      s1 <- letters
      s2 <- many letDigs
      return (s1:s2)
    where
      letters = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
      letDigs = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ ['0'..'9']

identParse :: Parser String
identParse = do n <- ident
                if n `elem` reserved then unexpected "illegal identifier"
                else return n

-- Grammar Implementation
prog :: Parser Program
prog = do
      p <- stms
      _ <- eof
      return (Prog p)

stms :: Parser [Stm]
stms = try stms1 <|> try stms2

stms1 :: Parser [Stm]
stms1 = do
      s1 <- lexeme stm
      void $ lexeme $ char ';'
      s2 <- lexeme stms
      return (s1:s2)

stms2 :: Parser [Stm]
stms2 = do
      s1 <- lexeme stm
      void  $ lexeme $ char ';'
      return [s1]

stm :: Parser Stm
stm =  try varDecl <|> try exprAsStm

varDecl :: Parser Stm
varDecl = try varDecl1 <|> try varDecl2

varDecl1 :: Parser Stm
varDecl1 = do
      void $ lexeme $ string "var"
      iden <- lexeme identParse
      a1   <- lexeme assignOpt
      return (VarDecl iden (Just a1))

varDecl2 :: Parser Stm
varDecl2 = do
      void $ lexeme $ string "var"
      iden <- lexeme identParse
      return (VarDecl iden Nothing)

assignOpt :: Parser Expr
assignOpt = do
      void $ lexeme $ char '='
      e1 <- lexeme expr
      return e1

exprAsStm :: Parser Stm
exprAsStm = do
      e1 <- lexeme expr
      return (ExprAsStm e1)

expr :: Parser Expr
expr = try exprExt <|> try exprEnd

exprExt :: Parser Expr
exprExt = do
      e1 <- lexeme expr1
      void $ lexeme $ char ','
      e2 <- lexeme expr
      return (Comma e1 e2)

exprEnd :: Parser Expr
exprEnd = lexeme expr1

-- Precedence for expressions
expr1 :: Parser Expr
expr1 = do
      rv <- r
      botbot rv

botbot :: Expr -> ParsecT String () Data.Functor.Identity.Identity Expr
botbot inval = do
      void $ lexeme $ string "==="
      rv <- r
      botbot (Call "===" [inval, rv])
  <|> return inval

r :: ParsecT String () Data.Functor.Identity.Identity Expr
r = do
      sv <- s
      bot sv

bot :: Expr -> ParsecT String () Data.Functor.Identity.Identity Expr
bot inval = (do
      void $ lexeme $ char '<'
      sv <- s
      bot (Call "<" [inval, sv]))
  <|> return inval

s :: ParsecT String () Data.Functor.Identity.Identity Expr
s = do
      tv <- t
      mid tv

mid :: Expr -> ParsecT String () Data.Functor.Identity.Identity Expr
mid inval = (do
      void $ lexeme $ char '+'
      tv <- t
      mid (Call "+" [inval, tv]))
  <|> (do
      void $ lexeme $ char '-'
      tv <- t
      mid (Call "-" [inval, tv]))
  <|> return inval

t :: ParsecT String () Data.Functor.Identity.Identity Expr
t = do
      pv <- prim
      top pv

top :: Expr -> ParsecT String () Data.Functor.Identity.Identity Expr
top inval = (do
      void $ lexeme $ char '*'
      pv <- prim
      top (Call "*" [inval, pv]))
  <|> (do
      void $ lexeme $ char '%'
      pv <- prim
      top (Call "%" [inval, pv]))
  <|> return inval

-- Try expressions
prim :: Parser Expr
prim = try number <|> try trueConst <|> try falseConst <|> try undef
      <|> try exprString <|> try array <|> try exprIdent <|> try parentheses

number :: Parser Expr
number = do
      d <- lexeme $ many1 digit
      return (Number (read d))

exprString :: Parser Expr
exprString = do
      spaces
      void $ char '\''
      ss <-  many1 letter
      void $ char '\''
      spaces
      return (String ss)

array :: Parser Expr
array = try array1 <|> try array2

array1 :: ParsecT String () Identity Expr
array1 = do
      void $ lexeme $ char '['
      e1 <- lexeme exprs
      void $ lexeme $ char ']'
      return (Array e1)

array2 :: Parser Expr
array2 = try array3 <|> try array4

array3 :: Parser Expr
array3 = do
      void $ lexeme $ char '['
      void $ lexeme $ string "for"
      void $ lexeme $ char '('
      iden1 <- lexeme identParse
      void $ lexeme $ string "of"
      e1   <- lexeme expr
      void $ lexeme $ char ')'
      a1   <- lexeme arrayCompr
      e2   <- lexeme expr
      void $ lexeme $ char ']'
      return (Compr (iden1, e1, Just a1) e2)

array4 :: Parser Expr
array4 = do
      void $ lexeme $ char '['
      void $ lexeme $ string "for"
      void $ lexeme $ char '('
      iden2 <- lexeme identParse
      void $ lexeme $ string "of"
      e1   <- lexeme expr
      void $ lexeme $ char ')'
      e2   <- lexeme expr
      void $ lexeme $ char ']'
      return (Compr (iden2, e1, Nothing) e2)

parentheses :: Parser Expr
parentheses = do
      void $ lexeme $ char '('
      e1 <- lexeme expr
      void $ lexeme $ char ')'
      return e1

undef :: Parser Expr
undef = do
      void $ lexeme $ string "undefined"
      return Undefined

trueConst :: Parser Expr
trueConst = do
      void $ lexeme $ string "true"
      return TrueConst

falseConst :: Parser Expr
falseConst = do
      void $ lexeme $ string "false"
      return FalseConst

exprIdent :: Parser Expr
exprIdent = do
      spaces
      iden <- lexeme identParse
      afterIdent iden

afterIdent :: Ident -> Parser Expr
afterIdent iden = try (afterIdent1 iden)
  <|> try (funCall iden)
  <|> return (Var iden)

afterIdent1 :: Ident -> Parser Expr
afterIdent1 iden =  do
      spaces
      void $ lexeme $ char '='
      e1 <- lexeme expr1
      return (Assign iden e1)

funCall :: FunName -> ParsecT String () Identity Expr
funCall iden = try (funCall1 iden) <|> try (funCall2 iden)

funCall1 :: FunName -> ParsecT String () Identity Expr
funCall1 iden = do
      void $ char '.'
      idAdd <- identParse
      let newId = iden ++ "." ++ idAdd
      funCall newId

funCall2 :: FunName -> ParsecT String () Identity Expr
funCall2 iden = do
      void $ lexeme $ char '('
      e1 <- lexeme exprs
      void $ lexeme $ char ')'
      return (Call iden e1)

exprs :: Parser [Expr]
exprs = expr1 `sepBy` lexeme (char ',')

arrayCompr :: Parser ArrayCompr
arrayCompr = try for1 <|> try for2 <|> try if1 <|> try if2

for1 :: Parser ArrayCompr
for1 = do
      void $ lexeme $ string "for"
      void $ lexeme $ char '('
      iden <- lexeme identParse
      void $ lexeme $ string "of"
      e1   <- lexeme expr
      void $ lexeme $ char ')'
      a1   <- lexeme arrayCompr
      return (ArrayForCompr (iden, e1, Just a1))

for2 :: Parser ArrayCompr
for2 = do
      void $ lexeme $ string "for"
      void $ lexeme $ char '('
      iden <- lexeme identParse
      void $ lexeme $ string "of"
      e1   <- lexeme expr
      void $ lexeme $ char ')'
      return (ArrayForCompr (iden, e1, Nothing))

if1 :: Parser ArrayCompr
if1 = do
      void $ lexeme $ string "if"
      void $ lexeme $ char '('
      e1 <- lexeme expr
      void $ lexeme $ char ')'
      a1 <- lexeme arrayCompr
      return (ArrayIf e1 (Just a1))

if2 :: Parser ArrayCompr
if2 = do
      void $ lexeme $ string "if"
      void $ lexeme $ char '('
      e1 <- lexeme expr
      void $ lexeme $ char ')'
      return (ArrayIf e1 Nothing)

parseString :: String -> Either ParseError Program
parseString = runParser prog () "Parsing error"

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = fmap parseString $ readFile path
