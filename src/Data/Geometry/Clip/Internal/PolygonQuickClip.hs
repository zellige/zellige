{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonQuickClip (
clipPolygonQc
, clipPolygonsQc
) where

import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

-- import qualified Data.Geometry.Clip.Internal.Line          as ClipLine
-- import qualified Data.Geometry.Clip.Internal.LineQuickClip as ClipLineQuickClip
import qualified Data.Geometry.Types.Geography as TypesGeography

clipPolygonsQc :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.Polygon -> Vector.Vector VectorTile.Polygon
clipPolygonsQc bb = Vector.foldl' addPoly mempty
  where
    addPoly acc f =
      case clipPolygonQc bb f of
        Nothing -> acc
        Just x  -> Vector.cons x acc

clipPolygonQc :: TypesGeography.BoundingBoxPts -> VectorTile.Polygon -> Maybe VectorTile.Polygon
clipPolygonQc bb (VectorTile.Polygon polyPoints inner) =
  case clipPolyPoints bb polyPoints of
    Nothing -> Nothing
    Just x  -> Just (VectorTile.Polygon x (clipPolygonsQc bb inner))

clipPolyPoints :: TypesGeography.BoundingBoxPts -> VectorStorable.Vector VectorTile.Point -> Maybe (VectorStorable.Vector VectorTile.Point)
clipPolyPoints bb polyPoints = checkLength (VectorStorable.uniq newClippedPoly)
  where
    newClippedPoly = foo (TypesGeography.bboxPtsToBbox bb) polyPoints
    checkLength newPoly =
      if VectorStorable.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

closeIfNot :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
closeIfNot poly =
  if lastPt /= firstPt
    then VectorStorable.cons lastPt poly
    else poly
  where
    lastPt = VectorStorable.last poly
    firstPt = VectorStorable.head poly

foo ::  TypesGeography.BoundingBox -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
foo _ polyPts = if VectorStorable.length polyPts <= 2 then VectorStorable.empty else newPoints
  where
    newPoints = undefined -- lineToClippedPoints bb (TypesGeography.newPointsToLines polyPts)
    -- newPoints = VectorStorable.foldl' (\pts polyLine -> clipEdges polyLine bbLine pts) VectorStorable.empty (TypesGeography.pointsToLines polyPts)

-- lineToClippedPoints :: TypesGeography.BoundingBox -> Vector.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector VectorTile.Point
-- lineToClippedPoints bb l = ClipLine.newFoldPointsToLine $ Vector.foldr (ClipLineQuickClip.clipOrDiscard bb) Vector.empty l

-- clip :: TypesGeography.BoundingBox -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
-- clip bb polyLines line =
--   VectorStorable.cons (lineToClippedPoints bb polyLines)

-- clipEdges :: TypesGeography.StorableLine -> TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
-- clipEdges polyLine@(TypesGeography.StorableLine s e) line acc =
--   case (inside e line, inside s line) of
--     (True, True)   -> VectorStorable.cons e acc
--     (True, False)  -> VectorStorable.cons e $ VectorStorable.cons (lineIntersectPoint line polyLine) acc
--     (False, True)  -> VectorStorable.cons (lineIntersectPoint line polyLine) acc
--     (False, False) -> acc

-- lineIntersectPoint :: TypesGeography.StorableLine -> TypesGeography.StorableLine -> VectorTile.Point
-- lineIntersectPoint (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) (TypesGeography.StorableLine (VectorTile.Point x1' y1') (VectorTile.Point x2' y2')) =
--   let
--     (dx, dy) = (x1 - x2, y1 - y2)
--     (dx', dy') = (x1' - x2', y1' - y2')
--     n1 = (x1 * y2) - (y1 * x2)
--     n2 = (x1' * y2') - (y1' * x2')
--     d = (dx * dy') - (dy * dx')
--     x = ((n1 * dx') - (n2 * dx)) `div` d
--     y = ((n1 * dy') - (n2 * dy)) `div` d
--   in VectorTile.Point x y

-- -- Is point of RHS of Line
-- inside :: VectorTile.Point -> TypesGeography.StorableLine -> Bool
-- inside (VectorTile.Point x y) (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = (x2 - x1) * (y - y1) > (y2 - y1) * (x - x1)
