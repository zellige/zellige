{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.Polygon (
  clipPolygon
, clipPolygons
, clipPolygonMap
, clipPolygonsMap
) where

import qualified Data.Aeson                    as Aeson
import qualified Data.Geospatial               as Geospatial
import qualified Data.LinearRing               as LinearRing
import qualified Data.List.NonEmpty            as ListNonEmpty
import qualified Data.Sequence                 as Sequence
import qualified Data.Validation               as Validation

import qualified Data.Geometry.Types.Geography as TypesGeography

clipPolygons :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygons bb polys (Geospatial.GeoFeature bbox _ props fId) acc =
  case clipPolygonsMap bb polys of
    Nothing   -> acc
    Just newPolys -> (Sequence.<|) (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon newPolys) props fId) acc

clipPolygonsMap :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Maybe Geospatial.GeoMultiPolygon
clipPolygonsMap bb (Geospatial.GeoMultiPolygon polys) =
  case maybeNewMultiPoly bb polys of
    Nothing       -> Nothing
    Just newPolys -> Just (Geospatial.GeoMultiPolygon newPolys)

maybeNewMultiPoly :: TypesGeography.BoundingBox -> Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
maybeNewMultiPoly bb = traverse $ traverse (clipLinearRing bb)

clipPolygon :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygon bb poly (Geospatial.GeoFeature bbox _ props fId) acc =
    case clipPolygonMap bb poly of
      Nothing   -> acc
      Just newPoly -> (Sequence.<|) (Geospatial.GeoFeature bbox (Geospatial.Polygon newPoly) props fId) acc

clipPolygonMap :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Maybe Geospatial.GeoPolygon
clipPolygonMap bb (Geospatial.GeoPolygon poly) =
  case maybeNewPoly bb poly of
    Nothing      -> Nothing
    Just newPoly -> Just (Geospatial.GeoPolygon newPoly)

maybeNewPoly :: TypesGeography.BoundingBox -> Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Maybe (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
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
    newLinearRing x = LinearRing.fromSeq (fmap Geospatial.GeoPointXY x) :: Validation.Validation (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS)) (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
    createNewClipPts = clipPolyPoints bb (fmap Geospatial.retrieveXY (LinearRing.toSeq linearRing))

clipPolyPoints :: TypesGeography.BoundingBox -> Sequence.Seq Geospatial.PointXY -> Maybe (Sequence.Seq Geospatial.PointXY)
clipPolyPoints bb polyPoints = checkLength newClippedPoly
  where
    newClippedPoly = Foldable.foldl' foo polyPoints (TypesGeography.mkBBoxPoly bb)
    checkLength newPoly =
      if Sequence.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

closeIfNot :: Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
closeIfNot poly@(firstPt Sequence.:<| (_ Sequence.:|> lastPt)) =
  if lastPt /= firstPt
    then lastPt Sequence.<| poly
    else poly

foo :: Sequence.Seq Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Sequence.Seq Geospatial.PointXY
foo polyPts bbLine = if Sequence.length polyPts <= 2 then Sequence.empty else newPoints
  where
    newPoints = Foldable.foldl' (\acc line -> clipEdges line bbLine acc) Sequence.empty (TypesGeography.pointsToLines polyPts)

clipEdges :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
clipEdges polyLine@(TypesGeography.GeoStorableLine s e) line acc =
  case (inside e line, inside s line) of
    (True, True)   -> (Sequence.<|) e acc
    (True, False)  -> (Sequence.<|) e $ (Sequence.<|) (lineIntersectPoint line polyLine) acc
    (False, True)  -> (Sequence.<|) (lineIntersectPoint line polyLine) acc
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
