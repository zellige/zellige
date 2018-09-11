{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.Polygon (
clipPolygon
, clipPolygons
) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

clipPolygons :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.Polygon -> Vector.Vector VectorTile.Polygon
clipPolygons bb = Vector.foldl' addPoly mempty
  where
    addPoly acc f =
      case clipPolygon bb f of
        Nothing -> acc
        Just x  -> Vector.cons x acc

clipPolygon :: TypesGeography.BoundingBoxPts -> VectorTile.Polygon -> Maybe VectorTile.Polygon
clipPolygon bb (VectorTile.Polygon polyPoints inner) =
  case clipPolyPoints bb polyPoints of
    Nothing -> Nothing
    Just x  -> Just (VectorTile.Polygon x (clipPolygons bb inner))

clipPolyPoints :: TypesGeography.BoundingBoxPts -> VectorStorable.Vector VectorTile.Point -> Maybe (VectorStorable.Vector VectorTile.Point)
clipPolyPoints bb polyPoints = checkLength (VectorStorable.uniq newClippedPoly)
  where
    newClippedPoly = VectorStorable.foldl' foo polyPoints (TypesGeography.mkBBoxPoly bb)
    checkLength newPoly =
      if VectorStorable.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

newClipPolyPoints :: TypesGeography.BoundingBox -> Vector.Vector Geospatial.PointXY -> Maybe (Vector.Vector Geospatial.PointXY)
newClipPolyPoints bb polyPoints = checkLength (Vector.uniq newClippedPoly)
  where
    newClippedPoly = Vector.foldl' newFoo polyPoints (TypesGeography.newMkBBoxPoly bb)
    checkLength newPoly =
      if Vector.length newPoly <= 2
        then Nothing
        else Just (newCloseIfNot newPoly)

closeIfNot :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
closeIfNot poly =
  if lastPt /= firstPt
    then VectorStorable.cons lastPt poly
    else poly
  where
    lastPt = VectorStorable.last poly
    firstPt = VectorStorable.head poly

newCloseIfNot :: Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
newCloseIfNot poly =
  if lastPt /= firstPt
    then Vector.cons lastPt poly
    else poly
  where
    lastPt = Vector.last poly
    firstPt = Vector.head poly

foo :: VectorStorable.Vector VectorTile.Point -> TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foo polyPts bbLine = if VectorStorable.length polyPts <= 2 then VectorStorable.empty else newPoints
  where
    newPoints = VectorStorable.foldl' (\pts polyLine -> clipEdges polyLine bbLine pts) VectorStorable.empty (TypesGeography.pointsToLines polyPts)

newFoo :: Vector.Vector Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY
newFoo polyPts bbLine = if Vector.length polyPts <= 2 then Vector.empty else newPoints
  where
    newPoints = Vector.foldl' (\pts polyLine -> newClipEdges polyLine bbLine pts) Vector.empty (TypesGeography.newPointsToLines polyPts)

clipEdges :: TypesGeography.StorableLine -> TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
clipEdges polyLine@(TypesGeography.StorableLine s e) line acc =
  case (inside e line, inside s line) of
    (True, True)   -> VectorStorable.cons e acc
    (True, False)  -> VectorStorable.cons e $ VectorStorable.cons (lineIntersectPoint line polyLine) acc
    (False, True)  -> VectorStorable.cons (lineIntersectPoint line polyLine) acc
    (False, False) -> acc

newClipEdges :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
newClipEdges polyLine@(TypesGeography.GeoStorableLine s e) line acc =
  case (newInside e line, newInside s line) of
    (True, True)   -> Vector.cons e acc
    (True, False)  -> Vector.cons e $ Vector.cons (newLineIntersectPoint line polyLine) acc
    (False, True)  -> Vector.cons (newLineIntersectPoint line polyLine) acc
    (False, False) -> acc

lineIntersectPoint :: TypesGeography.StorableLine -> TypesGeography.StorableLine -> VectorTile.Point
lineIntersectPoint (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) (TypesGeography.StorableLine (VectorTile.Point x1' y1') (VectorTile.Point x2' y2')) =
  let
    (dx, dy) = (x1 - x2, y1 - y2)
    (dx', dy') = (x1' - x2', y1' - y2')
    n1 = (x1 * y2) - (y1 * x2)
    n2 = (x1' * y2') - (y1' * x2')
    d = (dx * dy') - (dy * dx')
    x = ((n1 * dx') - (n2 * dx)) `div` d
    y = ((n1 * dy') - (n2 * dy)) `div` d
  in VectorTile.Point x y

newLineIntersectPoint :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Geospatial.PointXY
newLineIntersectPoint (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1' y1') (Geospatial.PointXY x2' y2')) =
  let
    (dx, dy) = (x1 - x2, y1 - y2)
    (dx', dy') = (x1' - x2', y1' - y2')
    n1 = (x1 * y2) - (y1 * x2)
    n2 = (x1' * y2') - (y1' * x2')
    d = (dx * dy') - (dy * dx')
    x = ((n1 * dx') - (n2 * dx)) / d
    y = ((n1 * dy') - (n2 * dy)) / d
  in Geospatial.PointXY x y

-- Is point of RHS of Line
inside :: VectorTile.Point -> TypesGeography.StorableLine -> Bool
inside (VectorTile.Point x y) (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = (x2 - x1) * (y - y1) >= (y2 - y1) * (x - x1)

newInside :: Geospatial.PointXY  -> TypesGeography.GeoStorableLine -> Bool
newInside (Geospatial.PointXY x y) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) = (x2 - x1) * (y - y1) >= (y2 - y1) * (x - x1)
