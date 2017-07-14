{-# LANGUAGE FlexibleContexts #-}

module Clip where

import qualified Data.Foldable                 as F
import qualified Data.Maybe                    as M
import qualified Data.Ord                      as O
import qualified Data.Tuple                    as T (swap)
import qualified Data.Vector                   as DV
import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG

import           Types

createBoundingBoxPts :: Integer -> ((Int, Int), (Int, Int))
createBoundingBoxPts extent = ((0, 0), (fromIntegral extent, fromIntegral extent))

createBoundingBoxPolygon :: Integer -> [VG.Point]
createBoundingBoxPolygon extent = [(0, 0), (0, fromIntegral extent), (fromIntegral extent, fromIntegral extent), (fromIntegral extent, 0), (0, 0)]

clipPoints :: (VG.Point, VG.Point) -> [VG.Point] -> [VG.Point]
clipPoints = filter . pointInsideExtent

clipLines :: (VG.Point, VG.Point) -> [VG.LineString] -> [VG.LineString]
clipLines bb lines = undefined

findOutcode bb lines = fmap evalDiffKeepSame <$> makeAPass bb lines
  where
    evalDiffKeepSame (a@(o1, p1), b@(o2, p2)) =
      case compare o1 o2 of
        GT -> (clipPoint o1 bb p1 p2, p2)
        LT -> (p1, clipPoint o2 bb p1 p2)
        EQ -> (p1, p2)

-- makeAPass :: (VG.Point, VG.Point) -> f VG.LineString -> f [((OutCode, t1), (OutCode, t))]
makeAPass bb lines = foldr (\l acc -> keepInRemoveOut l ++ acc) [] <$> xxx bb lines
  where
    keepInRemoveOut (a@(o1, _), b@(o2, _)) =
      case (o1, o2) of
        (Clip.Left, Clip.Left) -> []
        (Clip.Right, Clip.Right) -> []
        (Bottom, Bottom) -> []
        (Top, Top) -> []
        _ -> [(a, b)]

clipPoint outCode ((minx, miny), (maxx, maxy)) (x1, y1) (x2, y2) =
  case outCode of
    Clip.Top -> (x1 + (x2 - x1) * (maxy - y1) `div` (y2 - y1), maxy)
    Clip.Left -> (minx, y1 + (y2 - y1) * (minx - x1) `div` (x2 - x1))
    Clip.Right -> (maxx, y1 + (y2 - y1) * (maxx - x1) `div` (x2 - x1))
    Clip.Bottom -> (x1 + (x2 - x1) * (miny - y1) `div` (y2 - y1), miny)
    otherwise -> undefined

-- xxx :: (Functor f) => (VG.Point, VG.Point) -> f VG.LineString -> f [[(OutCode, String)]]
xxx bb = fmap (fmap (fmap (\(p1, p2) -> (toP1 bb p1, toP2 bb p2))) getLines)
  where
    getLines line = linesFromPoints . DVU.toList $ VG.lsPoints line
    toP1 bb p1 = (computeOutCode bb p1, p1)
    toP2 bb p2 = (computeOutCode bb p2, p2)

data OutCode = Inside | Left | Right | Bottom | Top deriving (Eq, Show, Ord)

instance Enum OutCode where
    fromEnum = M.fromJust . flip lookup table
    toEnum = M.fromJust . flip lookup (map T.swap table)
table = [(Inside, 0), (Clip.Left, 1), (Clip.Right, 2), (Bottom, 4), (Top, 8)]

computeOutCode :: (VG.Point, VG.Point) -> VG.Point -> OutCode
computeOutCode ((minX, minY), (maxX, maxY)) (x,y)
  | x < minX = Clip.Left
  | x > maxX = Clip.Right
  | y < minY = Clip.Top
  | y > maxY = Clip.Bottom
  | otherwise = Clip.Inside

clipPolygons :: [VG.Point] -> [VG.Polygon] -> [VG.Polygon]
clipPolygons bb = concatMap (pure . clipPolygon bb)

clipPolygon :: [VG.Point] -> VG.Polygon -> VG.Polygon
clipPolygon bb poly = VG.Polygon (DVU.fromList $ clip bb poly) mempty

clip :: [VG.Point] -> VG.Polygon -> [VG.Point]
clip bb poly = foldl (foo bb) (polyList poly) bbLines
  where
    bbLines = pointsToLines bb

foo :: [VG.Point] -> [VG.Point] -> (VG.Point, VG.Point) -> [VG.Point]
foo bb polyPts bbLine = foldl (\pts polyLine -> clipEdges polyLine bbLine ++ pts) [] (pointsToLines polyPts)

polyList :: VG.Polygon -> [VG.Point]
polyList p = DVU.toList $ VG.polyPoints p

pointsToLines :: [a] ->  [(a, a)]
pointsToLines pts = linesFromPoints (last pts : pts)

linesFromPoints :: [a] -> [(a, a)]
linesFromPoints = zip <*> tail

clipEdges :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> [VG.Point]
clipEdges polyLine@(s, e) clipLine =
  case (inside e clipLine, inside s clipLine) of
    (True, True)   -> [e]
    (True, False)  -> [e, lineIntersectPoint clipLine polyLine]
    (False, True)  -> [lineIntersectPoint clipLine polyLine]
    (False, False) -> []

lineIntersectPoint :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> VG.Point
lineIntersectPoint ((x1, y1), (x2, y2)) ((x1', y1'), (x2', y2')) =
    let
      (dx, dy) = (x1 - x2, y1 - y2)
      (dx', dy') = (x1' - x2', y1' - y2')
      n1 = (x1 * y2) - (y1 * x2)
      n2 = (x1' * y2') - (y1' * x2')
      d = (dx * dy' - dy * dx')
      x = (n1 * dx' - n2 * dx) `div` d
      y = (n1 * dy' - n2 * dy) `div` d
    in (x, y)

-- Is point of LHS of Line
inside :: VG.Point -> (VG.Point, VG.Point) -> Bool
inside (x, y) ((x1, y1), (x2, y2)) = (x2 - x1) * (y - y1) > (y2 - y1) * (x - x1)

-- Is point inside bounding box
pointInsideExtent :: (VG.Point, VG.Point) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

testPoly = clipPolygons clipPts [poly]
testSingle = clipPolygon clipPts poly
isOkay = VG.Polygon (DVU.fromList answer) mempty == testSingle
poly = VG.Polygon (DVU.fromList polyPts) mempty
polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
           (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
clipPts = [(100,100), (300,100), (300,300), (100,300)] :: [(Int,Int)]
bb = ((10,10),(60,60)) :: ((Int, Int), (Int, Int))
lines = [VG.LineString (DVU.fromList ([(11, 11), (59, 59)] :: [(Int,Int)])),
  VG.LineString (DVU.fromList ([(0, 0), (0, 100)] :: [(Int,Int)])),
  VG.LineString (DVU.fromList ([(0, 0), (70, 70)] :: [(Int,Int)]))]
answer = [(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]
-- [{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250}
--  {175 300} {125 300} {100 250}]

