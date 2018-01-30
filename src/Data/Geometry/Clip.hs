{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBoxPts
, clipPoints
, clipLines
, clipPolygon
, clipPolygons
) where

import qualified Data.Foldable             as DF
import qualified Data.Sequence             as DS
import qualified Data.Vector.Unboxed       as DVU
import qualified Geography.VectorTile      as VG
import           Prelude                   hiding (Left, Right, lines)

import           Data.Geometry.Types.Types

createBoundingBoxPts :: Word -> Word -> BoundingBoxPts
createBoundingBoxPts buffer extent = BoundingBoxPts (-iBuffer, -iBuffer) (iExtent+iBuffer, iExtent+iBuffer)
  where
    iBuffer = (fromIntegral buffer)
    iExtent = (fromIntegral extent)

clipPoints :: BoundingBoxPts -> DS.Seq VG.Point -> DS.Seq VG.Point
clipPoints = DS.filter . pointInsideExtent

clipLines :: BoundingBoxPts -> DS.Seq VG.LineString -> DS.Seq VG.LineString
clipLines bb lines = DF.foldl' maybeAddLine mempty outCodes
  where
    outCodes = findOutCode bb lines

clipPolygons :: BoundingBoxPts -> DS.Seq VG.Polygon -> DS.Seq VG.Polygon
clipPolygons bb = DF.foldl' addPoly mempty
  where
    addPoly acc f =
      case clipPolygon bb f of
        Nothing -> acc
        Just x  -> x DS.<| acc

clipPolygon :: BoundingBoxPts -> VG.Polygon -> Maybe VG.Polygon
clipPolygon bb poly@(VG.Polygon _ interiors) =
  case clip bb poly of
    Nothing -> Nothing
    Just x  -> Just (VG.Polygon x (clipPolygons bb interiors))

pointInsideExtent :: BoundingBoxPts -> VG.Point -> Bool
pointInsideExtent BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

maybeAddLine :: DS.Seq VG.LineString -> DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)) -> DS.Seq VG.LineString
maybeAddLine acc pp =
  case ((checkValidLineString . foldPointsToLine) pp) of
    Just res -> res DS.<| acc
    Nothing  -> acc
  where
    foldPointsToLine = DVU.foldr (mappend . (\((_,p1),(_,p2)) -> DVU.fromList [p1, p2])) mempty
    checkValidLineString pts =
      if DVU.length (segmentToLine pts) >= 2
        then Just (VG.LineString (segmentToLine pts))
        else Nothing

findOutCode :: Functor f => BoundingBoxPts -> f VG.LineString -> f (DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
findOutCode bb lines = fmap (DVU.filter isSame . DVU.map (evalDiffKeepSame bb)) (outCodeForLineStrings bb lines)

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: DVU.Vector VG.Point -> DVU.Vector VG.Point
segmentToLine l = if DVU.length l > 1 then DVU.cons start (second l) else mempty
  where
    start = DVU.head l
    second = DVU.ifilter (\i _ -> odd i)

evalDiffKeepSame :: BoundingBoxPts -> ((OutCode, VG.Point), (OutCode, VG.Point)) -> ((OutCode, VG.Point), (OutCode, VG.Point))
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

clipPoint :: OutCode -> BoundingBoxPts -> VG.Point -> VG.Point -> VG.Point
clipPoint outCode BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x1, y1) (x2, y2) =
  case outCode of
    Left   -> (minX, y1 + (y2 - y1) * (minX - x1) `div` (x2 - x1))
    Right  -> (maxX, y1 + (y2 - y1) * (maxX - x1) `div` (x2 - x1))
    Bottom -> (x1 + (x2 - x1) * (minY - y1) `div` (y2 - y1), minY)
    Top    -> (x1 + (x2 - x1) * (maxY - y1) `div` (y2 - y1), maxY)
    _      -> undefined

outCodeForLineStrings :: (Functor f) => BoundingBoxPts -> f VG.LineString -> f (DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
outCodeForLineStrings bb = fmap $ DVU.map out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints $ VG.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
linesFromPoints x = (DVU.zip <*> DVU.tail) (DVU.convert x)

outCodeForLine :: BoundingBoxPts -> VG.Point -> VG.Point -> ((OutCode, VG.Point), (OutCode, VG.Point))
outCodeForLine bb p1 p2 = (toP1, toP2)
  where
    toP1 = (computeOutCode bb p1, p1)
    toP2 = (computeOutCode bb p2, p2)

computeOutCode :: BoundingBoxPts -> VG.Point -> OutCode
computeOutCode BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x,y)
  | y > maxY  = Top
  | y < minY  = Bottom
  | x > maxX  = Right
  | x < minX  = Left
  | otherwise = Inside

clip :: BoundingBoxPts -> VG.Polygon -> Maybe (DVU.Vector VG.Point)
clip bb poly = checkLength (DVU.uniq newClippedPoly)
  where
    newClippedPoly = DVU.foldl' foo (VG.polyPoints poly) (mkBBoxPoly bb)
    checkLength newPoly =
      if DVU.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

closeIfNot :: Vector VG.Point -> Vector VG.Point
closeIfNot poly =
  if lastPt /= firstPt
    then DVU.cons lastPt poly
    else poly
  where
    lastPt = DVU.last poly
    firstPt = DVU.head poly

foo :: DVU.Vector VG.Point -> (VG.Point, VG.Point) -> DVU.Vector VG.Point
foo polyPts bbLine = if DVU.length polyPts <= 2 then DVU.empty else newPoints
  where
    newPoints = DVU.foldl' (\pts polyLine -> clipEdges polyLine bbLine pts) DVU.empty (pointsToLines polyPts)

clipEdges :: (VG.Point, VG.Point) -> (VG.Point, VG.Point) -> DVU.Vector VG.Point -> DVU.Vector VG.Point
clipEdges polyLine@(s, e) bb acc =
  case (inside e bb, inside s bb, lineIntersectPoint bb polyLine) of
    (False, False, _)    -> acc
    (True, True, _)      -> DVU.cons e acc
    (True, False, newPt) -> DVU.cons e $ DVU.cons newPt acc
    (False, True, newPt) -> DVU.cons newPt acc

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
