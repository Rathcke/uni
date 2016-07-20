module Curves where
import Text.Printf

data Point = Point (Double, Double)
    deriving Show

data Curve = Curve Point [Point]
    deriving Show

data Line = Vertical Double | Horizontal Double
    deriving Show

instance Eq Point where
    Point (x, y) == Point (x2, y2) = (abs(x-x2) < 0.01) && (abs(y-y2) < 0.01)

point :: (Double, Double) -> Point
point (x, y) = Point(x, y)

pointX :: Point -> Double
pointX (Point (x, _)) = x

pointY :: Point -> Double
pointY (Point (_, y)) = y

curve :: Point -> [Point] -> Curve
curve (Point (x, y)) (ps) = Curve (point (x, y)) ps

connect :: Curve -> Curve -> Curve
connect (Curve p ps) (Curve p2 ps2) = curve p (ps ++ [p2] ++ ps2)

rotate :: Curve -> Double -> Curve
rotate (Curve p ps) d =
    curve (rot p) (map rot ps) where
        rot p2 = point (pointX p2 * cos rad -pointY p2 * sin rad,
            pointX p2 * sin rad + pointY p2 * cos rad) where
                rad = d*pi/180

translate :: Curve -> Point -> Curve
translate (Curve p ps) p2 =
    curve (trans p) (map trans ps) where
        trans p3 = point (pointX p3 + chngX, pointY p3 + chngY) where
            chngX = pointX p2 - pointX p
            chngY = pointY p2 - pointY p

reflect :: Curve -> Line -> Curve
reflect (Curve p ps) (Horizontal d) =
    curve (ref p) (map ref ps) where
        ref p2 = point (pointX p2, pointY p2 - 2*(pointY p2 - d))
reflect (Curve p ps) (Vertical d)   =
    curve (ref p) (map ref ps) where
        ref p2 = point (pointX p2 - 2*(pointX p2 - d), pointY p2)

bbox :: Curve -> (Point, Point)
bbox (Curve p ps) =
    foldl (\(Point (x,y), Point (x2,y2)) (Point (x3, y3)) ->
        (Point (if x3 < x then x3 else x,
            if y3 < y then y3 else y),
         Point (if x3 > x2 then x3 else x2,
            if y3 > y2 then y3 else y2))) (p, p) ps

width :: Curve -> Double
width (Curve p ps) =
    let (Point (x,_), Point (x2,_)) = bbox (Curve p ps)
    in x2 - x

height :: Curve -> Double
height (Curve p ps) =
    let (Point (_,y), Point (_,y2)) = bbox (Curve p ps)
    in y2 - y

toList :: Curve -> [Point]
toList (Curve p ps) = p:ps

toSVG :: Curve -> String
toSVG (Curve p ps) =
    let zippedList = zip (p:ps) ps
    in
    "<svg xmlns='http://www.w3.org/2000/svg' width='10px' height='10px'" ++
    "version='1.1'> <g>" ++
    concatMap (\(Point (x, y), Point (x2, y2)) ->
    "<line style='stroke-width: 2px;" ++ " stroke: black; fill:white' x1='"  ++
    printf "%.2f" x ++ "' x2='" ++ printf "%.2f" x2 ++ "' y1='" ++
    printf "%.2f" y ++ "' y2='" ++ printf "%.2f" y2 ++"' />\n") zippedList ++
    "</g> </svg>"

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c)

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