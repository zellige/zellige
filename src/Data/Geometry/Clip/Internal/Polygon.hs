{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.Polygon (
clipPolygon
, clipPolygons
) where

import qualified Data.Foldable             as DF
import qualified Data.Sequence             as DS
import qualified Data.Vector.Unboxed       as DVU
import qualified Geography.VectorTile      as VG

import           Data.Geometry.Types.Types

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
