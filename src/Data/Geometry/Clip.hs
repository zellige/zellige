{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector

module Data.Geometry.Clip where

import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile.Geometry   as VG
import           Prelude                         hiding (Left, Right, lines)

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types

createBoundingBoxPts :: Pixels -> Pixels -> (VG.Point, VG.Point)
createBoundingBoxPts (Pixels buffer) (Pixels extent) = ((-buffer, -buffer), (extent+buffer, extent+buffer))

clipPoints :: (VG.Point, VG.Point) -> DV.Vector VG.Point -> DV.Vector VG.Point
clipPoints = DV.filter . pointInsideExtent

pointInsideExtent :: (VG.Point, VG.Point) -> VG.Point -> Bool
pointInsideExtent ((minX, minY), (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

clipLines :: (VG.Point, VG.Point) -> DV.Vector VG.LineString -> DV.Vector VG.LineString
clipLines bb lines = DV.map (createLineString . foldMap (\((_,p1),(_,p2)) -> DVU.fromList [p1, p2])) outCodes
  where
    createLineString x = VG.LineString (segmentToLine x)
    outCodes = findOutCode bb lines

findOutCode :: Functor f => (VG.Point, VG.Point) -> f VG.LineString -> f (DV.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
findOutCode bb lines = DV.filter isSame . fmap (evalDiffKeepSame bb) <$> outCodeForLineStrings bb lines

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: DVU.Vector VG.Point -> DVU.Vector VG.Point
segmentToLine l = if DVU.length l > 1 then DVU.cons start (second l) else mempty
  where
    start = DVU.head l
    second = DVU.ifilter (\i _ -> odd i)

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

clipPolygons :: (VG.Point, VG.Point) -> DV.Vector VG.Polygon -> DV.Vector VG.Polygon
clipPolygons bb = DV.foldl (\acc f -> DV.cons (clipPolygon bb f) acc) DV.empty

clipPolygon :: (VG.Point, VG.Point) -> VG.Polygon -> VG.Polygon
clipPolygon bb poly = VG.Polygon (clip bb poly) mempty

clip :: (VG.Point, VG.Point) -> VG.Polygon -> DVU.Vector VG.Point
clip bb poly = DVU.foldl foo (VG.polyPoints poly) (createClipPoly bb)

createClipPoly :: (VG.Point, VG.Point) -> DVU.Vector (VG.Point, VG.Point)
createClipPoly ((x1, y1), (x2, y2)) = pointsToLines $ DVU.fromList [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

myPts :: DVU.Vector (Int, Int)
myPts = DVU.fromList [(2712480,-1036479),(2713274,-1037797),(2714139,-1038761),(2714453,-1039514),(2714678,-1039662),(2714426,-1040008),(2714428,-1040392),(2715245,-1042453),(2713898,-1042680),(2714023,-1043453),(2713030,-1043609),(2713117,-1044494),(2711330,-1044752),(2711189,-1043881),(2710089,-1044063),(2709725,-1041780),(2708773,-1041295),(2707213,-1041499),(2706762,-1041217),(2706557,-1040019),(2707013,-1039496),(2708270,-1039212),(2709647,-1039668),(2709629,-1039276),(2710596,-1039112),(2710260,-1039364),(2710480,-1039720),(2710931,-1039643),(2710916,-1039477),(2712138,-1039988),(2713023,-1039860),(2713328,-1039433),(2713132,-1038583),(2712708,-1037829),(2711814,-1036762),(2712145,-1036498),(2712480,-1036479)]

myBB :: Data.Geometry.Types.BoundingBox
myBB = boundingBox (GoogleTileCoords 15 (Coords 28999 19781))

foo :: DVU.Vector VG.Point -> (VG.Point, VG.Point) -> DVU.Vector VG.Point
foo polyPts bbLine = DVU.foldl (\pts polyLine -> clipEdges polyLine bbLine DVU.++ pts) DVU.empty (pointsToLines polyPts)

pointsToLines :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
pointsToLines pts = (DVU.zip <*> DVU.tail) $ DVU.cons (DVU.last pts) pts

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DVU.Vector VG.Point -> DV.Vector (VG.Point, VG.Point)
linesFromPoints x = (DV.zip <*> DV.tail) (DV.convert x)

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
      d = (dx * dy') - (dy * dx')
      x = ((n1 * dx') - (n2 * dx)) `div` d
      y = ((n1 * dy') - (n2 * dy)) `div` d
    in (x, y)

-- Is point of RHS of Line
inside :: VG.Point -> (VG.Point, VG.Point) -> Bool
inside (x, y) ((x1, y1), (x2, y2)) = (x2 - x1) * (y - y1) > (y2 - y1) * (x - x1)
