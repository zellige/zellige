{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector

module Data.Geometry.Clip where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG
import           Prelude                       hiding (Left, Right, lines)

import           Data.Geometry.Types.Types

createBoundingBoxPts :: Pixels -> Pixels -> (VG.Point, VG.Point)
createBoundingBoxPts (Pixels buffer) (Pixels extent) = ((-iBuffer, -iBuffer), (iExtent+iBuffer, iExtent+iBuffer))
  where
    iBuffer = (fromIntegral . toInteger) buffer
    iExtent = (fromIntegral . toInteger) extent

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
    _                 -> True

clipPoint :: Integral a => OutCode -> ((a, a), (a, a)) -> (a, a) -> (a, a) -> (a, a)
clipPoint outCode ((minx, miny), (maxx, maxy)) (x1, y1) (x2, y2) =
  case outCode of
    Left   -> (minx, y1 + (y2 - y1) * (minx - x1) `div` (x2 - x1))
    Right  -> (maxx, y1 + (y2 - y1) * (maxx - x1) `div` (x2 - x1))
    Bottom -> (x1 + (x2 - x1) * (miny - y1) `div` (y2 - y1), miny)
    Top    -> (x1 + (x2 - x1) * (maxy - y1) `div` (y2 - y1), maxy)
    _      -> undefined

outCodeForLineStrings :: (Functor f) => (VG.Point, VG.Point) -> f VG.LineString -> f (DV.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
outCodeForLineStrings bb = fmap $ fmap out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints $ VG.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DVU.Vector VG.Point -> DV.Vector (VG.Point, VG.Point)
linesFromPoints x = (DV.zip <*> DV.tail) (DV.convert x)

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

simplifyPolygon :: VG.Polygon -> VG.Polygon
simplifyPolygon poly =
  VG.Polygon (DVU.uniq (quantizePoints 5 (VG.polyPoints poly))) DV.empty

quantizePoints :: Int -> DVU.Vector VG.Point -> DVU.Vector VG.Point
quantizePoints pixels = DVU.map (quantizePoint pixels)

quantizePoint :: Int -> VG.Point -> VG.Point
quantizePoint pixels (x, y) =
  let
    newX = (x `quot` pixels) * pixels
    newY = (y `quot` pixels) * pixels
  in (newX, newY)

clipPolygons :: (VG.Point, VG.Point) -> DV.Vector VG.Polygon -> DV.Vector VG.Polygon
clipPolygons bb = DV.foldl' addPoly DV.empty
  where
    addPoly acc f =
      case clipPolygon bb f of
        Nothing -> acc
        Just x  -> DV.cons x acc

clipPolygon :: (VG.Point, VG.Point) -> VG.Polygon -> Maybe VG.Polygon
clipPolygon bb poly =
  case clip bb poly of
    Nothing -> Nothing
    Just x  -> Just (VG.Polygon x DV.empty)

clip :: (VG.Point, VG.Point) -> VG.Polygon -> Maybe (DVU.Vector VG.Point)
clip bb poly = checkLength (DVU.uniq newClippedPoly)
  where
    newClippedPoly = DVU.foldl' foo (VG.polyPoints poly) (createClipPoly bb)
    checkLength newPoly = if DVU.null newPoly then Nothing else Just (DVU.cons (DVU.last newPoly) newPoly)

createClipPoly :: (VG.Point, VG.Point) -> DVU.Vector (VG.Point, VG.Point)
createClipPoly ((x1, y1), (x2, y2)) = pointsToLines $ DVU.fromList [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

foo :: DVU.Vector VG.Point -> (VG.Point, VG.Point) -> DVU.Vector VG.Point
foo polyPts bbLine = if DVU.null polyPts then DVU.empty else newPoints
  where
    newPoints = DVU.foldl' (\pts polyLine -> clipEdges polyLine bbLine pts) DVU.empty (pointsToLines polyPts)

pointsToLines :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
pointsToLines pts = (DVU.zip <*> DVU.tail) $ DVU.cons (DVU.last pts) pts

clipEdges :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> DVU.Vector VG.Point -> DVU.Vector VG.Point
clipEdges polyLine@(s, e) clipLine acc =
  case (inside e clipLine, inside s clipLine) of
    (True, True)   -> DVU.cons e acc
    (True, False)  -> DVU.cons e $ DVU.cons (lineIntersectPoint clipLine polyLine) acc
    (False, True)  -> DVU.cons (lineIntersectPoint clipLine polyLine) acc
    (False, False) -> acc

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
