module SalsaParser(parseFile, parseString) where

import SalsaAst as S
import SimpleParse
import Data.Char
import Control.Applicative
import Data.String.Utils

data Error = AmbiguousParse
           | NoParse
  deriving Show

reserved :: [String]
reserved = ["toggle", "hidden", "circle", "rectangle"]

colors :: [String]
colors = ["blue", "plum", "red", "green", "orange"]

--Convert string to color
toColor :: String -> Colour
toColor c = case c of
              "blue"   -> Blue
              "plum"   -> Plum
              "red"    -> Red
              "green"  -> Green
              _        -> Orange

-- Parse a color
color :: Parser Colour
color = do n <- ident
           if n `elem` colors then return (toColor n)
           else pfail

-- Parse an ident consisting of letters and digits separated by underscores.
-- An ident must start with a letter in order to be valid
ident :: Parser String
ident = token (do c   <- letter
                  cs <- letdigs
                  return (c:cs))
      where letter = satisfy isLetter
            num = satisfy isDigit
            underscore = satisfy (== '_')
            letdigs = many (letter <|> num <|> underscore )

-- Parse an ident if it isnt a reserved keyword
identParse :: Parser String
identParse = do n <- ident
                if n `elem` reserved then pfail
                else return n

-- Parse an integer
integer :: Parser Integer
integer = do int <- many1 (satisfy isDigit)
             return (read int :: Integer)

-- Move one or more shapes in paralleĺ
moveShape :: [String] -> Pos  -> Command
moveShape [] _ = undefined --TODO
moveShape [i] p = Move i p
moveShape (i:is) p = foldl (\cmd ids -> Par (Move ids p) cmd) (Move i p) is

-- Hide a shape
hideShape :: Command -> Command
hideShape s =
  case s of
    (Rect iden e1 e2 e3 e4 col _) -> Rect iden e1 e2 e3 e4 col False
    (Circ iden e1 e2 e3 col _)    -> Circ iden e1 e2 e3 col False
    _                             -> undefined --TODO

prog :: Parser [Command]
prog     =      coms
coms :: Parser [Command]
coms     =  (do c  <- com
                return [c])
        <|> (do c  <- com
                cs <- coms
                return (c:cs))
com :: Parser Command
com      =   do c  <- comt
                copt c
copt :: Command -> Parser Command
copt val =  (do symbol "||"
                c  <- comt
                copt (Par val c))
        <|>     return val

comt :: Parser Command
comt     =  (do ids   <- idents
                symbol "->"
                pos   <- position
                return $ moveShape ids pos)
        <|> (do symbol "toggle"
                iden  <- identParse
                return (Toggle iden))
        <|> (do symbol "hidden"
                shape <- shapeDef
                return $ hideShape shape)
        <|>     shapeDef
shapeDef :: Parser Command
shapeDef =  (do symbol "rectangle"
                iden <- identParse
                e1   <- expr
                e2   <- expr
                e3   <- expr
                e4   <- expr
                col  <- color
                return (Rect iden e1 e2 e3 e4 col True))
        <|> (do symbol "circle"
                iden <- identParse
                e1   <- expr
                e2   <- expr
                e3   <- expr
                col  <- color
                return (Circ iden e1 e2 e3 col True))
idents :: Parser [String]
idents   =  (do iden  <- identParse
                return [iden])
        <|> (do iden  <- identParse
                space
                idens <- idents
                return (iden:idens))
expr :: Parser Expr
expr           =   do tv <- t
                      exprOpt tv
exprOpt :: Expr -> Parser Expr
exprOpt inval  =  (do symbol "+"
                      tv <- t
                      exprOpt (Plus inval tv))
              <|> (do symbol "-"
                      tv <- t
                      exprOpt (Minus inval tv))
              <|>     return inval
t :: Parser Expr
t              = do pv <- prim
                    topt pv
topt :: Expr -> Parser Expr
topt inval     =  (do symbol "*"
                      pv <- prim
                      topt (Mult inval pv))
              <|> (do symbol "/"
                      pv <- prim
                      topt (Div inval pv))
              <|>     return inval
position :: Parser Pos
position =  (do symbol "("
                x <- expr
                symbol ","
                y <- expr
                symbol ")"
                return (Abs x y))
        <|> (do symbol "+"
                symbol "("
                x <- expr
                symbol ","
                y <- expr
                symbol ")"
                return (Rel x y))
prim :: Parser Expr
prim        =  (do spaces
                   pv   <- integer
                   return (S.Const pv))
           <|> (do iden <- identParse
                   symbol "."
                   symbol "x"
                   return (Xproj iden))
           <|> (do iden <- identParse
                   symbol "."
                   symbol "y"
                   return (Yproj iden))
           <|> (do symbol "("
                   ev   <- expr
                   symbol ")"
                   return ev)

parseString :: String -> Either Error Program
parseString str = case parseEof prog $ rstrip $ replace "\n" " " str of
                    [(res, _)] -> Right res
                    []         -> Left NoParse
                    _          -> Left AmbiguousParse

parseFile :: FilePath -> IO (Either Error Program)
parseFile fp = parseString <$> readFile fp
