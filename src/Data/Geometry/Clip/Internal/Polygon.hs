{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.Polygon (
  clipPolygon
, clipPolygons
) where

import qualified Data.Aeson                    as Aeson
import qualified Data.Geospatial               as Geospatial
import qualified Data.LinearRing               as LinearRing
import qualified Data.List.NonEmpty            as ListNonEmpty
import qualified Data.Validation               as Validation
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

clipPolygons :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygons bb (Geospatial.GeoMultiPolygon polys) (Geospatial.GeoFeature bbox _ props fId) acc =
  case maybeNewMultiPoly bb polys of
    Nothing   -> acc
    Just newPolys -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon newPolys)) props fId) acc

maybeNewMultiPoly :: TypesGeography.BoundingBox -> Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
maybeNewMultiPoly bb = traverse $ traverse (clipLinearRing bb)

clipPolygon :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygon bb (Geospatial.GeoPolygon poly) (Geospatial.GeoFeature bbox _ props fId) acc =
    case maybeNewPoly bb poly of
      Nothing   -> acc
      Just newPoly -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon newPoly)) props fId) acc

maybeNewPoly :: TypesGeography.BoundingBox -> Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Maybe (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
maybeNewPoly bb = traverse (clipLinearRing bb)

clipLinearRing :: TypesGeography.BoundingBox -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Maybe (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
clipLinearRing bb linearRing =
  case createNewClipPts of
    Nothing -> Nothing
    Just a ->
      case newLinearRing a of
        Validation.Failure _ -> Nothing
        Validation.Success b -> Just b
  where
    newLinearRing x = LinearRing.fromVector (fmap Geospatial.GeoPointXY x) :: Validation.Validation (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS)) (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
    createNewClipPts = clipPolyPoints bb (fmap Geospatial.retrieveXY (LinearRing.toVector linearRing))

clipPolyPoints :: TypesGeography.BoundingBox -> Vector.Vector Geospatial.PointXY -> Maybe (Vector.Vector Geospatial.PointXY)
clipPolyPoints bb polyPoints = checkLength (Vector.uniq newClippedPoly)
  where
    newClippedPoly = Vector.foldl' foo polyPoints (TypesGeography.newMkBBoxPoly bb)
    checkLength newPoly =
      if Vector.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

closeIfNot :: Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
closeIfNot poly =
  if lastPt /= firstPt
    then Vector.cons lastPt poly
    else poly
  where
    lastPt = Vector.last poly
    firstPt = Vector.head poly

foo :: Vector.Vector Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY
foo polyPts bbLine = if Vector.length polyPts <= 2 then Vector.empty else newPoints
  where
    newPoints = Vector.foldl' (\acc line -> clipEdges line bbLine acc) Vector.empty (TypesGeography.newPointsToLines polyPts)

clipEdges :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
clipEdges polyLine@(TypesGeography.GeoStorableLine s e) line acc =
  case (inside e line, inside s line) of
    (True, True)   -> Vector.cons e acc
    (True, False)  -> Vector.cons e $ Vector.cons (lineIntersectPoint line polyLine) acc
    (False, True)  -> Vector.cons (lineIntersectPoint line polyLine) acc
    (False, False) -> acc

lineIntersectPoint :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Geospatial.PointXY
lineIntersectPoint (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1' y1') (Geospatial.PointXY x2' y2')) =
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
inside :: Geospatial.PointXY  -> TypesGeography.GeoStorableLine -> Bool
inside (Geospatial.PointXY x y) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) = (x2 - x1) * (y - y1) >= (y2 - y1) * (x - x1)
