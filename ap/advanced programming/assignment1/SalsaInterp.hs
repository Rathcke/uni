module SalsaInterp(interpolate, runProg, Position) where

import Debug.Trace
import Data.Function
import qualified Control.Applicative as App
import Gpx
import SalsaAst
import Control.Monad
import Control.Arrow
-- import Text.Printf

type Position = (Integer, Integer)

data Shape   = Rectangle {pos::Position, w::Integer, h::Integer, color::Colour, 
                          vis::Bool, visited::Bool}
             | Circle    {pos::Position, r::Integer, color::Colour, vis::Bool,
                          visited::Bool}
             deriving (Show, Eq)
data Context = Context {ids::[(Ident, Shape)], framerate::Integer}
             deriving (Show, Eq)

newtype Salsa a = Salsa {runSalsa :: Context -> Either String (a, Context, Animation)}

instance Monad Salsa where
    -- return :: a  -> Salsa a
    return a = Salsa (\c -> Right(a, c, []))

    -- (>>=) :: Salsa a -> (a -> Salsa b) -> Salsa b
    (>>=) m f = Salsa (\ c -> case runSalsa m c of
                       Left str         -> Left str
                       Right (a, c', _) -> runSalsa (f a) c')

    fail str = Salsa (\_ -> Left str)

instance Functor Salsa where
    fmap = liftM

instance App.Applicative Salsa where
    pure = return
    (<*>) sf s = sf >>= \f -> fmap f s

-- Get context from curent state
getContext :: Salsa Context
getContext = Salsa (\c -> Right(c, c, []))

-- Put updated context into the Salsa monad
putContext :: Context -> Salsa ()
putContext c = Salsa (\_ -> Right((), c, []))

-- Put updated animation into the Salsa monad
putAnimation :: Animation -> Salsa ()
putAnimation a = Salsa (\c -> Right((), c, a))

-- Return a list of interpolated points between two points, based on a framerate
interpolate :: Integer -> Position -> Position -> [Position]
interpolate n pStart pEnd =
    let xDist = on (/) fromInteger (fst pEnd - fst pStart) n::Double
        yDist = on (/) fromInteger (snd pEnd - snd pStart) n::Double
    in  map (round Control.Arrow.*** round) $
        take (fi n) $
        iterate (\ (x, y) -> (x + xDist, y + yDist))
                (fi (fst pStart) + xDist, fi (snd pStart) + yDist) where
        fi x = fromInteger x

-- Execute a command
command :: Command -> Salsa ()
-- Create Rectangle with ID
command (Rect iden e1 e2 e3 e4 col v)  = do
    c <- getContext
    e1' <- evalExp e1
    e2' <- evalExp e2
    e3' <- evalExp e3
    e4' <- evalExp e4
    let rect  = Rectangle {pos = (e1', e2') , w = e3', h = e4', color = col, vis = v, visited = True}
    let c'    = Context {ids = (iden, rect) : ids c, framerate = framerate c}
    case lookup iden (ids c) of
      Just  _ -> fail "ID already exists"
      Nothing -> putContext c'
    let a = [[DrawRect e1' e2' e3' e4' (show col)]]
    putAnimation a
-- Create circle with ID
command (Circ iden e1 e2 e3 col v)     = do
    c <- getContext
    e1' <- evalExp e1
    e2' <- evalExp e2
    e3' <- evalExp e3
    let circ  = Circle {pos = (e1', e2') , r = e3', color = col, vis = v, visited = True}
    let c'    = Context {ids = (iden, circ) : ids c, framerate = framerate c}
    case lookup iden (ids c) of
      Just  _ -> fail "ID already exists"
      Nothing -> putContext c'
    let a = [[DrawCirc e1' e2' e3' (show col)]]
    putAnimation a
-- Move shape with ID=iden to absolute positive
command (Move iden (Abs x y))          = do
    c  <- getContext
    x1 <- evalExp (Xproj iden)
    y1 <- evalExp (Yproj iden)
    x2 <- evalExp x
    y2 <- evalExp y
    Control.Monad.when ( x2 < 0 || x2 > 400 || y2 < 0 || y2 > 400) $
      fail "New position is not valid"
    Control.Monad.when ( isVisited (ids c) iden ) $ 
      fail "Cannot manipulate shape mutiple times in 1 command"
    let ps = interpolate (framerate c) (x1, y1) (x2, y2)
    case lookup iden (ids c) of
      Just shape -> let updatedIds = updateID (ids c) (iden, shape {pos = (x2, y2)})
                    in  putContext Context {ids = updatedIds,
                                         framerate = framerate c}
      Nothing    -> fail "ID not found"
    case lookup iden (ids c) of
      Just shape -> putAnimation (generateAni ps shape)
      Nothing    -> fail "ID not found"
-- Move shape with ID=iden to relative position
command (Move iden (Rel x y))          = do
    c  <- getContext
    x1 <- evalExp (Xproj iden)
    y1 <- evalExp (Yproj iden)
    x2 <- evalExp (Plus (Const x1) x)
    y2 <- evalExp (Plus (Const y1) y)
    Control.Monad.when ( x2 < 0 || x2 > 400 || y2 < 0 || y2 > 400) $
      fail "New position is not valid"
    Control.Monad.when ( isVisited (ids c) iden ) $ 
      fail "Cannot manipulate shape mutiple times in 1 command"
    let ps = interpolate (framerate c) (x1, y1) (x2, y2)
    case lookup iden (ids c) of
      Just shape -> let updatedIds = updateID (ids c) (iden, shape {pos = (x2, y2)})
                    in  putContext Context {ids = updatedIds,
                                         framerate = framerate c}
      Nothing    -> fail "ID not found"
    case lookup iden (ids c) of
      Just shape -> putAnimation (generateAni ps shape)
      Nothing    -> fail "ID not found"
command (Toggle iden)                  = do
    c <- getContext
    Control.Monad.when ( isVisited (ids c) iden ) $ 
      fail "Cannot manipulate shape mutiple times in 1 command"
    let newIds =  map (\(i, s) -> if i == iden
                                  then (i, s {vis = not(vis s), visited = True})
                                  else (i, s) ) $ ids c
    {-genPoints (pos c
    case lookup iden (ids c) of
      Just shape -> if vis shape then generateAni () shape
                    else undefined
      Nothing    -> fail "ID not found"
    -}
    let c'     = Context {ids = newIds, framerate = framerate c}
    putContext c'
command (Par c1 c2)                    = do
    c <- getContext
    let (c1Con, c1Ani) = case runSalsa (command c1) c of
                              Right (_, con, ani) -> (con, ani)
                              -- TODO Handle error
                              Left s              -> trace ("c1 "++ show s ) undefined
    let (c2Con, c2Ani) = case runSalsa (command c2) c1Con of
                              Right (_, con, ani) -> (con, ani)
                              -- TODO Handle error
                              Left s              -> trace ("c2 "++ show s ) undefined
    --(_, c2Con, c2Ani) <- undefined --runSalsa (command c2) c1Con     
    putContext c2Con
    putAnimation (mergeFrames c1Ani c2Ani)

-- Update a shape in the ID map
updateID :: [(Ident, Shape)] -> (Ident, Shape) -> [(Ident, Shape)]
updateID idList (iden, s) = map (\(ident, shape) -> if ident == iden
                                                    then (iden, s {visited = True})
                                                    else (ident, shape)) idList

-- Check if a shape has been modified this key frame
isVisited :: [(Ident, Shape)] -> Ident -> Bool
isVisited idList ident = case lookup ident idList of
                           Just shape   -> visited shape
                           -- TODO Handle error
                           Nothing      -> undefined

-- Reset the status of any visited shapes, for the next key frame
resetVisited :: [(Ident, Shape)] -> [(Ident, Shape)] 
resetVisited idList = map (\ (ident, shape) -> (ident, shape {visited = False}) ) idList

-- Merge the animations when doing commands executed in parallel
mergeFrames :: [[a]] -> [[a]] -> [[a]]
mergeFrames [] []         = []
mergeFrames [] (y:ys)     = y : mergeFrames [] ys
mergeFrames (x:xs) []     = x : mergeFrames xs []
mergeFrames (x:xs) (y:ys) = (x ++ y) : mergeFrames xs ys

--generatePoints pos frames = 

-- Generate the animation for the respective shape, based on the interpolated points
generateAni :: [Position] -> Shape -> [Frame]
generateAni [] _                           = []
generateAni (p:ps) (Rectangle _ wdt hgt c v vs) =
    if not v
    then []
    else [uncurry DrawRect p wdt hgt (show c)] : generateAni ps (Rectangle p wdt hgt c v vs)
generateAni (p:ps) (Circle _ rad c v vs)      =
    if not v
    then []
    else [uncurry DrawCirc p rad (show c)] : generateAni ps (Circle p rad c v vs)

-- Evaluate an expression, returning a Salsa Integer
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
evalExp (Xproj iden)  = Salsa (\c -> case lookup iden $ ids c of
                                          Just s  -> Right (fst $ pos s, c, [])
                                          Nothing -> Left "ID not found")
evalExp (Yproj iden)  = Salsa (\c -> case lookup iden $ ids c of
                                          Just s  -> Right (snd $ pos s, c, [])
                                          Nothing -> Left "ID not found")

-- Execute a program given a framerate and a series of commands
runProg :: Integer -> Program -> Either String Animation
runProg frames program =
  let initialContext = Context { ids = [], framerate = frames}
      (_, animation) = foldl (\(context, Right a) com ->
        case runSalsa (command com) context of
          Right (_, cntxt, ani) -> (cntxt {ids = resetVisited (ids cntxt)}, Right (a ++ ani))
          Left str              -> (context, Left str))
        (initialContext, Right []) program
  in animation
