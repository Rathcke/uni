module SalsaInterp(interpolate, runProg, Position) where

import Data.Function
import qualified Control.Applicative as App
import Gpx
import SalsaAst
-- import Text.Printf

type Position = (Integer, Integer)

data Shape   = Rectangle {pos::Position, w::Integer, h::Integer, color::Colour, vis::Bool}
             | Circle    {pos::Position, r::Integer, color::Colour, vis::Bool}
             deriving (Show, Eq)
data Context = Context {ids::[(Ident, Shape)], framerate::Integer}
             deriving (Show, Eq)

newtype Salsa a = Salsa {runSalsa :: Context -> Either String (a, Context, Animation)}

instance Monad Salsa where
    -- return :: a  -> Salsa a
    return a = Salsa (\c -> Right(a, c, []))

    -- (>>=) :: Salsa a -> (a -> Salsa b) -> Salsa b
    (>>=) m f = Salsa (\ c -> case (runSalsa m c) of
                       Left str         -> Left str
                       Right (a, c', _) -> runSalsa (f a) c')

    fail str = Salsa (\_ -> Left str)

instance Functor Salsa where
    fmap f s = s >>= return . f

instance App.Applicative Salsa where
    pure = return
    (<*>) sf s = sf >>= \f -> fmap f s

getContext :: Salsa Context
getContext = Salsa (\c -> Right(c, c, []))

putContext :: Context -> Salsa ()
putContext c = Salsa (\_ -> Right((), c, []))

putAnimation :: Animation -> Salsa ()
putAnimation a = Salsa (\c -> Right((), c, a))

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n pStart pEnd =
    let xDist = (on (/) fromInteger (fst pEnd - fst pStart) n)::Double
        yDist = (on (/) fromInteger (snd pEnd - snd pStart) n)::Double
    in  map (\(x, y) -> (round x, round y)) $
        take (fi n) $
        iterate (\ (x, y) -> (x + xDist, y + yDist))
                ((fi $ fst pStart) + xDist, (fi $ snd pStart) + yDist) where
        fi x = fromInteger x

command :: Command -> Salsa ()
command (Rect iden e1 e2 e3 e4 col v)  = do
    c <- getContext
    e1' <- evalExp e1
    e2' <- evalExp e2
    e3' <- evalExp e3
    e4' <- evalExp e4
    let rect  = Rectangle {pos = (e1', e2') , w = e3', h = e4', color = col, vis = v}
    let c'    = Context {ids = (iden, rect) : ids c, framerate = framerate c}
    case lookup iden (ids c) of
      Just  _ -> fail "ID already exists"
      Nothing -> putContext c'
    let a = [[DrawRect e1' e2' e3' e4' (show col)]]
    putAnimation a
command (Circ iden e1 e2 e3 col v)     = do
    c <- getContext
    e1' <- evalExp e1
    e2' <- evalExp e2
    e3' <- evalExp e3
    let circ  = Circle {pos = (e1', e2') , r = e3', color = col, vis = v}
    let c'    = Context {ids = (iden, circ) : ids c, framerate = framerate c}
    case lookup iden (ids c) of
      Just  _ -> fail "ID already exists"
      Nothing -> putContext c'
    let a = [[DrawCirc e1' e2' e3' (show col)]]
    putAnimation a
command (Move iden (Abs x y))          = do
    c  <- getContext
    x1 <- evalExp (Xproj iden)
    y1 <- evalExp (Yproj iden)
    x2 <- evalExp x
    y2 <- evalExp y
    if (x2 < 0 || x2 > 400 || y2 < 0 || y2 > 400)
      then fail "New position is not valid"
      else return ()
    let ps = interpolate (framerate c) (x1, y1) (x2, y2)
    case lookup iden (ids c) of
      Just shape -> let updatedIds = updateID (ids c) (iden, shape {pos = (x2, y2)})
                    in  putContext (Context {ids = updatedIds,
                                         framerate = framerate c})
      Nothing    -> fail "ID not found"
    case lookup iden (ids c) of
      Just shape -> putAnimation (generateAni ps shape)
      Nothing    -> fail "ID not found"
command (Move iden (Rel x y))          = do
    c  <- getContext
    x1 <- evalExp (Xproj iden)
    y1 <- evalExp (Yproj iden)
    x2 <- evalExp (Plus (Const x1) x)
    y2 <- evalExp (Plus (Const y1) y)
    if (x2 < 0 || x2 > 400 || y2 < 0 || y2 > 400)
      then fail "New position is not valid"
      else return()
    let ps = interpolate (framerate c) (x1, y1) (x2, y2)
    case lookup iden (ids c) of
      Just shape -> let updatedIds = updateID (ids c) (iden, shape {pos = (x2, y2)})
                    in  putContext (Context {ids = updatedIds,
                                         framerate = framerate c})
      Nothing    -> fail "ID not found"
    case lookup iden (ids c) of
      Just shape -> putAnimation (generateAni ps shape)
      Nothing    -> fail "ID not found"
command (Toggle iden)                  = do
    c <- getContext
    let newIds =  map (\(i, s) -> if (i == iden)
                                  then (if (vis s == False)
                                       then  (i, s {vis = True})
                                       else  (i, s {vis = False}))
                                  else (i, s) ) $ ids c
    let c'     = Context {ids = newIds, framerate = framerate c}
    putContext c'
command (Par c1 c2)                    = do
    c        <- getContext
    resolvC1 <- command c1
    resolvC2 <- command c2
    undefined

updateID :: [(Ident, Shape)] -> (Ident, Shape) -> [(Ident, Shape)]
updateID idList (iden, s) = map (\(ident, shape) -> if ident == iden
                                                    then (iden, s)
                                                    else (ident, shape)) idList

generateAni :: [Position] -> Shape -> [Frame]
generateAni [] _                           = []
generateAni (p:ps) (Rectangle _ wdt hgt c v) =
    if (v == False)
    then []
    else [DrawRect (fst p) (snd p) wdt hgt (show c)] : generateAni ps (Rectangle p wdt hgt c v)
generateAni (p:ps) (Circle _ rad c v)      =
    if (v == False)
    then []
    else [DrawCirc (fst p) (snd p) rad (show c)] : generateAni ps (Circle p rad c v)

evalExp :: Expr -> Salsa Integer
evalExp (Plus e1 e2)  = do
    x <- evalExp e1
    y <- evalExp e2
    return $ x + y
evalExp (Minus e1 e2) = do
    x <- evalExp e1
    y <- evalExp e2
    return $ x - y
evalExp (Mult e1 e2)  = do
    x <- evalExp e1
    y <- evalExp e2
    return $ x * y
evalExp (Div e1 e2)   = do
    x <- evalExp e1
    y <- evalExp e2
    if y == 0 then fail "Cant divide by 0" else return $ div x y
evalExp (Const n)     = return n
evalExp (Xproj iden)  = Salsa (\c -> case (lookup iden $ ids c) of
                                          Just s  -> Right (fst $ pos s, c, [])
                                          Nothing -> Left "ID not found")
evalExp (Yproj iden)  = Salsa (\c -> case (lookup iden $ ids c) of
                                          Just s  -> Right (snd $ pos s, c, [])
                                          Nothing -> Left "ID not found")

runProg :: Integer -> Program -> Either String Animation
runProg frames program =
  let initialContext = Context { ids = [], framerate = frames}
      (_, animation) = foldl (\(context, Right a) com -> 
        case (runSalsa (command com) context ) of 
          Right (_, cntxt, ani) -> (cntxt, Right (a ++ ani))
          Left str              -> (context, Left str))
        (initialContext, Right []) program
  in animation
