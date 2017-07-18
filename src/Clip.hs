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

type BoundingBox = (VG.Point, VG.Point)

createBoundingBoxPts :: Int -> Clip.BoundingBox
createBoundingBoxPts extent = ((0, 0), (extent, extent))

clipPoints :: Clip.BoundingBox -> [VG.Point] -> [VG.Point]
clipPoints = filter . pointInsideExtent

pointInsideExtent :: (VG.Point, VG.Point) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

clipLines :: Clip.BoundingBox -> [VG.LineString] -> [VG.LineString]
clipLines bb lines = fmap (createLineString . foldMap (\((_,p1),(_,p2)) -> [p1, p2])) outCodes
  where
    createLineString = (VG.LineString . DVU.fromList) . segmentToLine
    outCodes = findOutCode bb lines

findOutCode :: Functor f => (VG.Point, VG.Point) -> f VG.LineString -> f [((OutCode, VG.Point), (OutCode, VG.Point))]
findOutCode bb lines = filter isSame . fmap (evalDiffKeepSame bb) <$> outCodeForLineStrings bb lines

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
    (Clip.Left   , Clip.Left  ) -> False
    (Clip.Right  , Clip.Right ) -> False
    (Clip.Bottom , Clip.Bottom) -> False
    (Clip.Top    , Clip.Top   ) -> False
    _ -> True

clipPoint :: Integral a => OutCode -> ((a, a), (a, a)) -> (a, a) -> (a, a) -> (a, a)
clipPoint outCode ((minx, miny), (maxx, maxy)) (x1, y1) (x2, y2) =
  case outCode of
    Clip.Left   -> (minx, y1 + (y2 - y1) * (minx - x1) `div` (x2 - x1))
    Clip.Right  -> (maxx, y1 + (y2 - y1) * (maxx - x1) `div` (x2 - x1))
    Clip.Bottom -> (x1 + (x2 - x1) * (miny - y1) `div` (y2 - y1), miny)
    Clip.Top    -> (x1 + (x2 - x1) * (maxy - y1) `div` (y2 - y1), maxy)
    otherwise -> undefined

outCodeForLineStrings :: (Functor f) => (VG.Point, VG.Point) -> f VG.LineString -> f [((OutCode, VG.Point), (OutCode, VG.Point))]
outCodeForLineStrings bb = fmap $ fmap out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints (DVU.toList $ VG.lsPoints line)

outCodeForLine :: (Ord a, Ord b) => ((a, b), (a, b)) -> (a, b) -> (a, b) -> ((OutCode, (a, b)), (OutCode, (a, b)))
outCodeForLine bb p1 p2 = (toP1 bb p1, toP2 bb p2)
  where
    toP1 bb p1 = (computeOutCode bb p1, p1)
    toP2 bb p2 = (computeOutCode bb p2, p2)

data OutCode = Inside | Left | Right | Bottom | Top deriving (Eq, Show, Ord)

computeOutCode :: (Ord a, Ord b) => ((a, b), (a, b)) -> (a, b) -> OutCode
computeOutCode ((minX, minY), (maxX, maxY)) (x,y)
  | y > maxY  = Clip.Top
  | y < minY  = Clip.Bottom
  | x > maxX  = Clip.Right
  | x < minX  = Clip.Left
  | otherwise = Clip.Inside

clipPolygons :: Clip.BoundingBox -> [VG.Polygon] -> [VG.Polygon]
clipPolygons bb = concatMap (pure . clipPolygon bb)

clipPolygon :: Clip.BoundingBox -> VG.Polygon -> VG.Polygon
clipPolygon bb poly = VG.Polygon (DVU.fromList $ clip bb poly) mempty

clip :: Clip.BoundingBox -> VG.Polygon -> [VG.Point]
clip ((x1, y1), (x2, y2)) poly = foldl (foo bb) (polyList poly) bbLines
  where
    bb = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
    bbLines = pointsToLines bb

foo :: [VG.Point] -> [VG.Point] -> (VG.Point, VG.Point) -> [VG.Point]
foo bb polyPts bbLine = foldl (\pts polyLine -> clipEdges polyLine bbLine ++ pts) [] (pointsToLines polyPts)

polyList :: VG.Polygon -> [VG.Point]
polyList p = DVU.toList $ VG.polyPoints p

pointsToLines :: [a] ->  [(a, a)]
pointsToLines pts = linesFromPoints (last pts : pts)

linesFromPoints :: [a] -> [(a, a)]
linesFromPoints = zip <*> tail

segmentToLine :: [a] -> [a]
segmentToLine a@(x:_) = x : second a
  where
    second (_:y:xs) = y : second xs
    second _ = []
segmentToLine _ = []

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

testPoly = clipPolygons clipPts [poly]
testSingle = clipPolygon clipPts poly
isOkay = VG.Polygon (DVU.fromList answer) mempty == testSingle
poly = VG.Polygon (DVU.fromList polyPts) mempty
polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
           (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
clipPts = ((100, 100), (300, 300))
linesBbTst = ((10,10),(60,60)) :: ((Int, Int), (Int, Int))
linesTst = [VG.LineString (DVU.fromList ([(11, 11), (59, 59)] :: [(Int,Int)])),
  VG.LineString (DVU.fromList ([(0, 0), (0, 100)] :: [(Int,Int)])),
  VG.LineString (DVU.fromList ([(5, 5), (45, 50), (90, 140)] :: [(Int,Int)])),
  VG.LineString (DVU.fromList ([(0, 0), (60, 60)] :: [(Int,Int)]))]
answer = [(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]
-- [{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250}
--  {175 300} {125 300} {100 250}]

