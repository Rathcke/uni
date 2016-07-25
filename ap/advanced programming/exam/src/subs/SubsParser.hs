module SubsParser (
  ParseError(..),
  parseString,
  parseFile
) where


import SubsAst as P
import SimpleParse as S
import Data.Char
import Control.Applicative
import Data.String.Utils

data ParseError = ParseError String
                deriving (Show, Eq)

{-- Public API --}
parseString :: String -> Either ParseError Program
parseString str = case parseEof prog $ rstrip $ replace "\n" " " str of
                    [(res, _)] -> Right res
                    []         -> Left (ParseError "NoParse")
                    _          -> Left (ParseError "AmbiguousParse")


parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path
{----------------}

-- Reserved keywords
reserved :: [String]
reserved = ["var", "true", "false", "undefined", "for", "of", "if"]

-- Parse an ident consisting of alphanumeric characters, possibly separated by underscores.
-- An ident can start with a underscore in order to be valid
ident :: Parser String
ident = token (do c  <- letterunder
                  cs <- letdigsund
                  return (c:cs))
      where letter = satisfy isLetter
            num = satisfy isDigit
            underscore = satisfy (== '_')
            letterunder = letter <|> underscore
            letdigsund = many (letter <|> num <|> underscore )

-- Parse an ident if it isnt a reserved keyword
identParse :: Parser String
identParse = do n <- ident
                if n `elem` reserved then pfail
                else return n

-- Parse an ident consisting of alphanumeric characters, possibly separated by underscores.
-- An ident can start with a underscore in order to be valid
funIdent :: Parser String
funIdent =    (do c  <- letterunder
                  cs <- letdigsund
                  return (c:cs))
      where letter = satisfy isLetter
            num = satisfy isDigit
            underscore = satisfy (== '_')
            letterunder = letter <|> underscore
            letdigsund = many (letter <|> num <|> underscore )

-- Parse an ident if it isnt a reserved keyword
fidentParse :: Parser String
fidentParse = do n <- funIdent
                 if n `elem` reserved then pfail
                 else return n


strings :: Parser String
strings = token (do c <- quote
                    cs <- ascii
                    css <- quote
                    return (c:cs ++ [css]))
        where quote = satisfy (== '\'')
              ascii = many1 (satisfy (/= '\''))

stringParse :: Parser String
stringParse = do n <- strings
                 if n `elem` reserved then pfail
                 else return $ stripChars "\'" n

stripChars :: String -> String -> String
stripChars = filter . flip notElem

allOrNothing :: Expr -> Maybe Expr
allOrNothing e = case e of
  Assign _ a -> Just a
  _          -> Nothing

-- Parse an integer
integer :: Parser Int
integer = token (do pre <- neg
                    post <- digits
                    if length(pre : post) > 8 then pfail
                    else return $ read $ pre : post)
        where neg = satisfy (== '-') <|> satisfy isDigit
              digits = many (satisfy isDigit)

prog :: Parser Program
prog            =  do s <- stms
                      return (Prog s)

stms :: Parser [Stm]
stms            =   (do s <- stm
                        symbol ";"
                        ss <- stms
                        return (s:ss))
               <|>      return []

stm :: Parser Stm
stm             =  (do symbol "var"
                       iden <- identParse
                       val <- assign iden
                       return $ VarDecl iden (allOrNothing val))
               <|> (do ev <- expr
                       return $ ExprAsStm ev)

assign :: String -> Parser Expr
assign inval    = (do symbol "="
                      ev <- expr
                      return $ Assign inval ev)
               <|>    return (Var inval)
-- Expression parsing
expr :: Parser Expr
expr           =  do ev <- expr1
                     exprOpt ev
-- Comma expressions
exprOpt :: Expr -> Parser Expr
exprOpt inval  =  (do symbol ","
                      ev <- expr1
                      exprOpt $ Comma inval ev)
              <|>     return inval


expr1 :: Parser Expr
expr1          =  (do tv <- t1
                      eeq tv)
              <|> (do iden <- identParse
                      eass iden)

-- Assign expression
ease :: Expr -> Parser Expr
ease inval     =  (do iden <-identParse
                      eass iden)
              <|>     return inval
-- Assign string
eass :: String -> Parser Expr
eass inval      = do symbol "="
                     ev <- expr1
                     ease $ Assign inval ev
-- Compare equality
eeq :: Expr -> Parser Expr
eeq inval      =  (do symbol "==="
                      tv <- t1
                      eeq $ Call "===" [inval, tv])
              <|>     return inval

t1 :: Parser Expr
t1             = do tv <- t2
                    elt tv

-- Less-than expression
elt :: Expr -> Parser Expr
elt inval      =  (do symbol "<"
                      tv <- t2
                      elt $ Call "<" [inval, tv])
              <|>     return inval

t2 :: Parser Expr
t2             = do tv <- t3
                    epm tv

-- Plus-minus expressions
epm :: Expr -> Parser Expr
epm inval      =  (do symbol "+"
                      tv <- t3
                      epm (Call "+" [inval, tv]))
              <|> (do symbol "-"
                      tv <- t3
                      epm (Call "-" [inval, tv]))
              <|>     return inval

t3 :: Parser Expr
t3             =  do fv <- f
                     emm fv

-- Multiply-modulo expression
emm :: Expr -> Parser Expr
emm inval      =  (do symbol "*"
                      fv <- f
                      emm (Call "*" [inval, fv]))
              <|> (do symbol "%"
                      fv <- f
                      emm (Call "%" [inval, fv]))
              <|>     return inval

f :: Parser Expr
f              =  (do pv   <- integer
                      return $ P.Number pv)
              <|> (do stringp <- stringParse
                      return $ P.String stringp)
              <|> (do symbol "undefined"
                      return P.Undefined)
              <|> (do symbol "true"
                      return P.TrueConst)
              <|> (do symbol "false"
                      return P.FalseConst)
              <|> (do iden <- identParse
                      afterIdent iden)
              <|> (do symbol "("
                      ev <- expr
                      symbol ")"
                      return ev)
              <|> (do symbol "["
                      esv <- exprs
                      symbol "]"
                      return $ Array esv)
              <|> (do symbol "["
                      symbol "for"
                      symbol "("
                      iden <- identParse
                      symbol "of"
                      e1v <- expr
                      symbol ")"
                      arr <- arrCompr
                      e2v <- expr
                      symbol "]"
                      return $ Compr (iden, e1v, arr) e2v)
-- Array expressions
exprs :: Parser [Expr]
exprs  =  (do ev <- expr1
              comexprs ev)
      <|>     return []
--CommaExprs
comexprs :: Expr -> Parser [Expr]
comexprs inval =  (do symbol ","
                      ev <- expr1
                      esv <- comexprs ev
                      return (inval : esv))
              <|>     return [inval]
--AfterIdent
afterIdent ::  String -> Parser Expr
afterIdent inval =      funcall inval
                <|>     return (Var inval)
-- Function calls
funcall :: String -> Parser Expr
funcall inval    =  (do char '.'
                        iden <- fidentParse
                        funcall $ inval ++ "." ++ iden)
                <|> (do symbol "("
                        ev <- exprs
                        symbol ")"
                        return (Call inval ev))
--ArrayCompr
arrCompr :: Parser (Maybe ArrayCompr)
arrCompr         =  (do symbol "if"
                        symbol "("
                        ev <- expr
                        symbol ")"
                        arrv <- arrCompr
                        return $ Just $ ArrayIf ev arrv)
                <|> (do symbol "for"
                        symbol "("
                        iden <- identParse
                        symbol "of"
                        ev <- expr
                        symbol ")"
                        arrv <- arrCompr
                        return $ Just $ ArrayForCompr (iden, ev, arrv))
                <|>     return Nothing
