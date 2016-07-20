module SalsaParser where

import Text.Parsec
import Text.Parsec.String
import Control.Monad
import SalsaAst

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

program :: Parser [Command]
program = do
  many1 command

command :: Parser Command
command = do
  try move <|> try toggle <|> try shapeDef <|> try shapeDefH <|> try par

par :: Parser Command
par = do
  c1 <- lexeme $ command
  void $ lexeme $ string "||"
  c2 <- lexeme $ command
  return (Par c1 c2)

toggle :: Parser Command
toggle = do
  void $ lexeme $ string "toggle"
  iden <- lexeme $ ident
  return (Toggle iden)

move :: Parser Command
move = do
  iden <- lexeme $ ident
  void $ lexeme $ string "->"
  p    <- lexeme $ pos
  return (Move iden p)

shapeDefH :: Parser Command
shapeDefH = do
  void $ lexeme $ string "hidden"
  try rectH <|> try circH

rectH :: Parser Command
rectH = do
  void $ lexeme $ string "rectangle"
  iden <- lexeme $ ident
  e1   <- lexeme $ expr
  e2   <- lexeme $ expr
  e3   <- lexeme $ expr
  e4   <- lexeme $ expr
  c    <- lexeme $ colour
  return (Rect iden e1 e2 e3 e4 c False)

circH :: Parser Command
circH = do
  void $ lexeme $ string "circle"
  iden <- lexeme $ ident
  e1   <- lexeme $ expr
  e2   <- lexeme $ expr
  e3   <- lexeme $ expr
  c    <- lexeme $ colour
  return (Circ iden e1 e2 e3 c False)

shapeDef :: Parser Command
shapeDef = do
 try rect <|> try circ

rect :: Parser Command
rect = do
  void $ lexeme $ string "rectangle"
  iden <- lexeme $ ident
  e1   <- lexeme $ expr
  e2   <- lexeme $ expr
  e3   <- lexeme $ expr
  e4   <- lexeme $ expr
  c    <- lexeme $ colour
  return (Rect iden e1 e2 e3 e4 c True)

circ :: Parser Command
circ = do
  void $ lexeme $ string "circle"
  iden <- lexeme $ ident
  e1   <- lexeme $ expr
  e2   <- lexeme $ expr
  e3   <- lexeme $ expr
  c    <- lexeme $ colour
  return (Circ iden e1 e2 e3 c True)

ident :: Parser String
ident = do
  whitespace
  s <- many1 letter
  case s of
    "rectangle" -> unexpected "rectangle"
    "circle"    -> unexpected "circle"
    "hidden"    -> unexpected "hidden"
    "toggle"    -> unexpected "toggle"
    _           -> return s

pos :: Parser Pos
pos = do
  try abso <|> try rela

abso :: Parser Pos
abso = do
  void $ lexeme $ char '('
  e1 <- expr
  void $ lexeme $ char ','
  e2 <- expr
  void $ lexeme $ char ')'
  return (Abs e1 e2)

rela :: Parser Pos
rela = do
  void $ lexeme $ char '+'
  void $ lexeme $ char '('
  e1 <- expr
  void $ lexeme $ char ','
  e2 <- expr
  void $ lexeme $ char ')'
  return (Rel e1 e2)

expr :: Parser Expr
expr = do
  try mult <|> try divi <|> try plus <|> try minus <|> try prim

mult :: Parser Expr
mult = do
  p <- prim
  void $ lexeme $ char '*'
  e <- expr
  return (Mult p e)

divi :: Parser Expr
divi = do
  p <- prim
  void $ lexeme $ char '/'
  e <- expr
  return (Div p e)

plus :: Parser Expr
plus = do
  p <- prim
  void $ lexeme $ char '+'
  e <- expr
  return (Plus p e)

minus :: Parser Expr
minus = do
  p <- prim
  void $ lexeme $ char '-'
  e <- expr
  return (Minus p e)

prim :: Parser Expr
prim = do
  n <- lexeme $ many1 digit
  return (Const (read n))

colour :: Parser Colour
colour = do
  whitespace
  c <- many1 letter
  if c == "blue" || c == "plum" || c == "red" || c == "green" || c == "orange"
  then return (toColour c)
  else unexpected "colour"

toColour :: String -> Colour
toColour s = case s of
             "blue"   -> Blue
             "plum"   -> Plum
             "red"    -> Red
             "green"  -> Green
             "orange" -> Orange
             _        -> undefined

parseString :: Parser a -> String -> Either ParseError a
parseString s = parse s ""

--parseFile :: FilePath -> IO (Either ParseError Program)
--parseFile fp = fmap parseString $ readFile fp
