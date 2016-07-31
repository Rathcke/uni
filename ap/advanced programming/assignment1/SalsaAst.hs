module SalsaAst where

type Program = [Command]
data Command = Rect Ident Expr Expr Expr Expr Colour Bool
             | Circ Ident Expr Expr Expr Colour Bool
             | Move Ident Pos
             | Toggle Ident
             | Par Command Command
             deriving (Show, Eq)
data Pos = Abs Expr Expr
         | Rel Expr Expr
         deriving (Show, Eq)
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Const Integer
          | Xproj Ident
          | Yproj Ident
          deriving (Show, Eq)
data Colour = Blue | Plum | Red | Green | Orange
            deriving (Show, Eq)
type Ident = String
