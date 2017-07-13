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

clipPolygon :: [(VG.Point, VG.Point)] -> VG.Polygon -> VG.Polygon
clipPolygon bbLines poly = foldl (\acc x -> clipPolygonWithLine x acc) poly bbLines

clipPolygonWithLine :: (VG.Point, VG.Point) -> VG.Polygon -> VG.Polygon
clipPolygonWithLine line p = VG.Polygon (DVU.fromList $ xxx line p ) DV.empty

xxx :: (VG.Point, VG.Point) -> VG.Polygon -> [VG.Point]
xxx line p = last foo : foo
  where
    foo = concatMap (`lineLHS` line) (linesFromPolygon p)

linesFromPolygon :: VG.Polygon -> [(VG.Point, VG.Point)]
linesFromPolygon p = linesFromPoints . DVU.toList $ VG.polyPoints p

linesFromPoints :: [a] -> [(a, a)]
linesFromPoints = zip <*> tail

-- Is line within polygon
lineLHS :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> [VG.Point]
lineLHS l1@(p1, p2) l2 =
  case (pointLHS p1 l2, pointLHS p2 l2) of
    (False, False) -> []
    (True, True) -> [p2]
    (False, True) -> [lineIntersectPoint l1 l2, p2]
    (True, False) -> [lineIntersectPoint l1 l2]

lineIntersectPoint :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> VG.Point
lineIntersectPoint ((x1, y1), (x2, y2)) ((x1', y1'), (x2', y2')) =
    let (r, s) = (x1 * y2 - y1 * x2, x1' * y2' - y1' * x2')
        (t, u, v, w) = (x1 - x2, y1' - y2', y1 - y2, x1' - x2')
        d = t * u - v * w
        x = (r * w - t * s) `div` d
        y = (r * u - v * s) `div` d
    in (x, y)

-- Is point of LHS of Line
pointLHS :: VG.Point -> (VG.Point, VG.Point) -> Bool
pointLHS (x, y) ((x1, y1), (x2, y2)) = (x2 - x1) * (y - y1) >= (y2 - y1) * (x - x1)

-- Is point inside bounding box
pointInsideExtent :: ((Int, Int), (Int, Int)) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

testPoly = clipPolygons clipPts [poly]
testSingle = clipPolygon (linesFromPoints clipPts) poly
poly = VG.Polygon (DVU.fromList polyPts) DV.empty
polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
           (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
clipPts = [(100,100), (300,100), (300,300), (100,300)] :: [(Int,Int)]
