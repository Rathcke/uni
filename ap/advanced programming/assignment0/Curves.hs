module Curves(Point, Curve, Line(..),
              point, pointX, pointY,
              curve, connect, rotate,
              translate, reflect, bbox,
              height, width, toList,
              toSVG, toFile, hilbert) where

import Text.Printf

data Point = Point (Double, Double)
  deriving (Show)

data Curve = Curve Point [Point]
  deriving (Eq, Show)

data Line = Vertical Double | Horizontal Double
  deriving (Show)

instance Eq Point where
  Point(x1,y1) == Point(x2,y2) = abs(x1 - x2) < 0.01 && abs(y1 -y2) < 0.01

point :: (Double, Double) -> Point
point (x, y) = Point (x, y)

pointX :: Point -> Double
pointX (Point (x, _)) = x

pointY :: Point -> Double
pointY (Point (_, y)) = y

curve :: Point -> [Point] -> Curve
curve = Curve

connect :: Curve -> Curve -> Curve
connect (Curve p1 cs1) (Curve p2 cs2) = curve p1 (cs1 ++ [p2] ++ cs2)

rotate :: Curve -> Double -> Curve
rotate (Curve p cs) r = curve (rot p) (map rot cs)
  where rot rotp = let rad  = (r * pi) / 180
                       xnew = pointX rotp * cos rad - pointY rotp * sin rad
                       ynew = pointX rotp * sin rad + pointY rotp * cos rad
                    in point (xnew, ynew)

translate :: Curve -> Point -> Curve
translate (Curve p cs) newp = curve (trans p) (map trans cs)
  where trans tp = let distx = pointX newp - pointX p
                       disty = pointY newp - pointY p
                    in point (pointX tp + distx, pointY tp + disty)

reflect :: Curve -> Line -> Curve
reflect (Curve p cs) l = curve (ref p) (map ref cs)
  where ref refp = case l of
                     Vertical d   -> point(2*d - pointX refp, pointY refp)
                     Horizontal d -> point(pointX refp, 2*d - pointY refp)

bbox :: Curve -> (Point, Point)
bbox (Curve p cs) = (point (xmin, ymin), point (xmax, ymax))
  where (xmin, ymin) = foldl (\ (m,n) (Point (a,b)) -> (min a m, min b n))
                         (pointX p, pointY p) (p:cs)
        (xmax, ymax) = foldl (\ (m,n) (Point (a,b)) -> (max a m, max b n))
                         (pointX p, pointY p) (p:cs)

height :: Curve -> Double
height c = pointY p2 - pointY p1
  where (p1, p2) =  bbox c

width :: Curve -> Double
width c = pointX p2 - pointX p1
  where (p1, p2) =  bbox c

toList :: Curve -> [Point]
toList (Curve c cs) = c:cs

svgLines :: [Point] -> String
svgLines [] = ""
svgLines (_:[]) = ""
svgLines (p:ps:pss) =  "\t\t<line style=\"stroke-width: 2px; stroke: black; fill:white\"" ++
                       " x1=\"" ++ printf "%.2f" (pointX p) ++ "\" x2=\"" ++
                       printf "%.2f" (pointX ps) ++ "\" y1=\"" ++ printf "%.2f" (pointY p) ++
                       "\" y2=\"" ++ printf "%.2f" (pointY ps) ++ "\"></line>\n" ++
                       svgLines (ps:pss)

toSVG :: Curve -> String
toSVG c = let w =  printf "%d" ((ceiling $ height c)::Integer)
              h =  printf "%d" ((ceiling $ height c)::Integer)
              svgStart = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++
                         w ++ "px\" height=\"" ++ h ++ "px\" version=\"1.1\">\n\t<g>\n"
              svgEnd   = "\t</g>\n</svg>\n"
           in svgStart ++ svgLines (toList c) ++ svgEnd

toFile :: Curve -> FilePath -> IO ()
toFile c p = writeFile p (toSVG c)

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

