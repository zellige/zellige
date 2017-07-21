{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector

module Clip where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG
import           Prelude                       hiding (Left, Right, lines)

import           Types

type BoundingBox = (VG.Point, VG.Point)

createBoundingBoxPts :: Pixels -> Clip.BoundingBox
createBoundingBoxPts (Pixels extent) = ((0, 0), (extent, extent))

clipPoints :: Clip.BoundingBox -> DV.Vector VG.Point -> DV.Vector VG.Point
clipPoints = DV.filter . pointInsideExtent

pointInsideExtent :: (VG.Point, VG.Point) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

clipLines :: Clip.BoundingBox -> DV.Vector VG.LineString -> DV.Vector VG.LineString
clipLines bb lines = DV.map (createLineString . foldMap (\((_,p1),(_,p2)) -> DVU.fromList [p1, p2])) outCodes
  where
    createLineString x = VG.LineString (segmentToLine x)
    outCodes = findOutCode bb lines

findOutCode :: Functor f => (VG.Point, VG.Point) -> f VG.LineString -> f (DV.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
findOutCode bb lines = DV.filter isSame . fmap (evalDiffKeepSame bb) <$> outCodeForLineStrings bb lines

evalDiffKeepSame :: Integral a => ((a, a), (a, a)) -> ((OutCode, (a, a)), (OutCode, (a, a))) -> ((OutCode, (a, a)), (OutCode, (a, a)))
evalDiffKeepSame bb (a@(o1, p1), b@(o2, p2)) =
  case compare o1 o2 of
    GT -> eval (clipAndCompute o1, b)
    LT -> eval (a, clipAndCompute o2)
    EQ -> (a, b)
  where
    eval = evalDiffKeepSame bb
    clipAndCompute o = computeNewOutCode $ clipPoint o bb p1 p2
    computeNewOutCode p = (computeOutCode bb p, p)

isSame :: ((OutCode, a), (OutCode, b)) -> Bool
isSame ((o1, _), (o2, _)) =
  case (o1, o2) of
    (Left   , Left  ) -> False
    (Right  , Right ) -> False
    (Bottom , Bottom) -> False
    (Top    , Top   ) -> False
    _ -> True

clipPoint :: Integral a => OutCode -> ((a, a), (a, a)) -> (a, a) -> (a, a) -> (a, a)
clipPoint outCode ((minx, miny), (maxx, maxy)) (x1, y1) (x2, y2) =
  case outCode of
    Left   -> (minx, y1 + (y2 - y1) * (minx - x1) `div` (x2 - x1))
    Right  -> (maxx, y1 + (y2 - y1) * (maxx - x1) `div` (x2 - x1))
    Bottom -> (x1 + (x2 - x1) * (miny - y1) `div` (y2 - y1), miny)
    Top    -> (x1 + (x2 - x1) * (maxy - y1) `div` (y2 - y1), maxy)
    _ -> undefined

outCodeForLineStrings :: (Functor f) => (VG.Point, VG.Point) -> f VG.LineString -> f (DV.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
outCodeForLineStrings bb = fmap $ fmap out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints $ VG.lsPoints line

outCodeForLine :: (Ord a, Ord b) => ((a, b), (a, b)) -> (a, b) -> (a, b) -> ((OutCode, (a, b)), (OutCode, (a, b)))
outCodeForLine bb p1 p2 = (toP1, toP2)
  where
    toP1 = (computeOutCode bb p1, p1)
    toP2 = (computeOutCode bb p2, p2)

computeOutCode :: (Ord a, Ord b) => ((a, b), (a, b)) -> (a, b) -> OutCode
computeOutCode ((minX, minY), (maxX, maxY)) (x,y)
  | y > maxY  = Top
  | y < minY  = Bottom
  | x > maxX  = Right
  | x < minX  = Left
  | otherwise = Inside

clipPolygons :: Clip.BoundingBox -> DV.Vector VG.Polygon -> DV.Vector VG.Polygon
clipPolygons bb = DV.foldl (\acc f -> DV.cons (clipPolygon bb f) acc) DV.empty

clipPolygon :: Clip.BoundingBox -> VG.Polygon -> VG.Polygon
clipPolygon bb poly = VG.Polygon (clip bb poly) mempty

clip :: Clip.BoundingBox -> VG.Polygon -> DVU.Vector VG.Point
clip ((x1, y1), (x2, y2)) poly = DVU.foldl foo (VG.polyPoints poly) bbLines
  where
    bb = DVU.fromList [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
    bbLines = pointsToLines bb

foo :: DVU.Vector VG.Point -> (VG.Point, VG.Point) -> DVU.Vector VG.Point
foo polyPts bbLine = DVU.foldl (\pts polyLine -> clipEdges polyLine bbLine DVU.++ pts) DVU.empty (pointsToLines polyPts)

pointsToLines :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
pointsToLines pts = (DVU.zip <*> DVU.tail) $ DVU.cons (DVU.last pts) pts

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DVU.Vector VG.Point -> DV.Vector (VG.Point, VG.Point)
linesFromPoints x = (DV.zip <*> DV.tail) (DV.convert x)

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: DVU.Vector VG.Point -> DVU.Vector VG.Point
segmentToLine l = if DVU.length l > 1 then DVU.cons start (second l) else mempty
  where
    start = DVU.head l
    second = DVU.ifilter (\i _ -> odd i)

clipEdges :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> DVU.Vector VG.Point
clipEdges polyLine@(s, e) clipLine =
  case (inside e clipLine, inside s clipLine) of
    (True, True)   -> DVU.singleton e
    (True, False)  -> DVU.fromList [e, lineIntersectPoint clipLine polyLine]
    (False, True)  -> DVU.singleton (lineIntersectPoint clipLine polyLine)
    (False, False) -> DVU.empty

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

-- testPoly = clipPolygons clipPts [poly]
-- testSingle = clipPolygon clipPts poly
-- isOkay = VG.Polygon (DVU.fromList answer) mempty == testSingle
-- poly = VG.Polygon (DVU.fromList polyPts) mempty
-- polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
--            (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
-- clipPts = ((100, 100), (300, 300))
-- linesBbTst = ((10,10),(60,60)) :: ((Int, Int), (Int, Int))
-- linesTst = [VG.LineString (DVU.fromList ([(11, 11), (59, 59)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(0, 0), (0, 100)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(5, 5), (45, 50), (90, 140)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(0, 0), (60, 60)] :: [(Int,Int)]))]
-- answer = [(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]
-- [{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250}
--  {175 300} {125 300} {100 250}]

