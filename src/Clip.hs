-- Based on https://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping#Haskell
-- Seems wrong.

module Clip where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG

import           Types

createBoundingBoxPts :: Integer -> ((Int, Int), (Int, Int))
createBoundingBoxPts extent = ((0, 0), (fromIntegral extent, fromIntegral extent))

createBoundingBoxPolygon :: Integer -> [VG.Point]
createBoundingBoxPolygon extent = [(0, 0), (0, fromIntegral extent), (fromIntegral extent, fromIntegral extent), (fromIntegral extent, 0), (0, 0)]

clipPoints :: ((Int, Int), (Int, Int)) -> [VG.Point] -> [VG.Point]
clipPoints = filter . pointInsideExtent

clipLines :: ((Int, Int), (Int, Int)) -> [VG.LineString] -> [VG.LineString]
clipLines = undefined

clipPolygons :: [VG.Point] -> [VG.Polygon] -> [VG.Polygon]
clipPolygons bb polys = undefined -- concatMap (\bbLine -> fmap (clipPolygon bbLine) polys) polys bbLines
  where
    bbLines = linesFromPolygon (VG.Polygon (DVU.fromList bb) DV.empty)

clipPolygon :: [VG.Point] -> VG.Polygon -> VG.Polygon
clipPolygon bb poly = VG.Polygon (DVU.fromList $ clip bb poly) mempty

clip :: [VG.Point] -> VG.Polygon -> [VG.Point]
clip bb poly = foldl foo (polyList poly) bbLines
    where
      foo polyPts bbLine = foldl (\acc2 polyLine -> clipEdges polyLine bbLine ++ acc2) [] (newLinesFromPts polyPts)
      newLinesFromPts polyPts = linesFromPoints (last polyPts : polyPts)
      bbLines = linesFromPoints (last bb : bb)

linesFromPolygon :: VG.Polygon -> [(VG.Point, VG.Point)]
linesFromPolygon p = linesFromPoints (polyList p)

polyList :: VG.Polygon -> [VG.Point]
polyList p = last polys : polys
  where
    polys = DVU.toList $ VG.polyPoints p

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
pointInsideExtent :: ((Int, Int), (Int, Int)) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

testPoly = clipPolygons clipPts [poly]
testSingle = clipPolygon clipPts poly
poly = VG.Polygon (DVU.fromList polyPts) DV.empty
polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
           (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
clipPts = [(100,100), (300,100), (300,300), (100,300)] :: [(Int,Int)]
-- [{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250}
--  {175 300} {125 300} {100 250}]
