{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonSutherlandHodgman (
  clipPolygonSh
, clipPolygonsSh
, clipPolygonMapSh
, clipPolygonsMapSh
) where

import qualified Data.Aeson                          as Aeson
import qualified Data.Foldable                       as Foldable
import qualified Data.Geospatial                     as Geospatial
import qualified Data.LinearRing                     as LinearRing
import qualified Data.List.NonEmpty                  as ListNonEmpty
import qualified Data.Maybe                          as Maybe
import qualified Data.Sequence                       as Sequence
import qualified Data.Validation                     as Validation

import qualified Data.Geometry.Clip.Internal.Polygon as ClipPolygon
import qualified Data.Geometry.Types.Geography       as TypesGeography

clipPolygonsSh :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygonsSh bb polys (Geospatial.GeoFeature bbox _ props fId) acc =
  case clipPolygonsMapSh bb polys of
    Nothing      -> acc
    Just newPolys -> Geospatial.GeoFeature bbox (Geospatial.MultiPolygon newPolys) props fId Sequence.<| acc

clipPolygonsMapSh :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Maybe Geospatial.GeoMultiPolygon
clipPolygonsMapSh bb (Geospatial.GeoMultiPolygon polys) =
  if Sequence.null newMultiPolys
    then Nothing
    else Just (Geospatial.GeoMultiPolygon newMultiPolys)
  where
    newMultiPolys = clippedMultiPoly bb polys

-- TODO Don't use traverse here either - just filter out nothings
clippedMultiPoly :: TypesGeography.BoundingBox -> Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
clippedMultiPoly bb = fmap (clippedPoly bb)

clipPolygonSh :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygonSh bb poly (Geospatial.GeoFeature bbox _ props fId) acc =
    case clipPolygonMapSh bb poly of
      Nothing      -> acc
      Just newPoly -> Geospatial.GeoFeature bbox (Geospatial.Polygon newPoly) props fId Sequence.<| acc

clipPolygonMapSh :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Maybe Geospatial.GeoPolygon
clipPolygonMapSh bb (Geospatial.GeoPolygon poly) =
  if Sequence.null newPolys
    then Nothing
    else Just (Geospatial.GeoPolygon newPolys)
  where
    newPolys = clippedPoly bb poly

clippedPoly :: TypesGeography.BoundingBox -> Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
clippedPoly bb = Foldable.foldl' (\acc x -> Maybe.maybe acc (Sequence.<| acc) (clipLinearRing bb x)) Sequence.empty

clipLinearRing :: TypesGeography.BoundingBox -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Maybe (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
clipLinearRing bb linearRing =
  case createNewClipPts of
    Nothing -> Nothing
    Just a ->
      case newLinearRing a of
        Validation.Failure _ -> Nothing
        Validation.Success b -> Just b
  where
    newLinearRing x = LinearRing.fromSeq (fmap Geospatial.GeoPointXY x) :: Validation.Validation (ListNonEmpty.NonEmpty (LinearRing.SequenceToLinearRingError Geospatial.GeoPositionWithoutCRS)) (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
    createNewClipPts = clipPolyPoints bb (fmap Geospatial.retrieveXY (LinearRing.toSeq linearRing))

clipPolyPoints :: TypesGeography.BoundingBox -> Sequence.Seq Geospatial.PointXY -> Maybe (Sequence.Seq Geospatial.PointXY)
clipPolyPoints bb polyPoints = ClipPolygon.closeIfNot newClippedPoly
  where
    newClippedPoly = Foldable.foldl' foo polyPoints (TypesGeography.mkBBoxPoly bb)

foo :: Sequence.Seq Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Sequence.Seq Geospatial.PointXY
foo polyPts bbLine = if Sequence.length polyPts <= 2 then Sequence.empty else newPoints
  where
    newPoints = Foldable.foldl' (\acc line -> clipEdges line bbLine acc) Sequence.empty (TypesGeography.pointsToLines polyPts)

clipEdges :: TypesGeography.GeoStorableLine -> TypesGeography.GeoStorableLine -> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
clipEdges polyLine@(TypesGeography.GeoStorableLine s e) line acc =
  case (inside e line, inside s line) of
    (True, True)   -> e Sequence.<| acc
    (True, False)  -> (Sequence.<|) e $ (Sequence.<|) (lineIntersectPoint line polyLine) acc
    (False, True)  -> lineIntersectPoint line polyLine Sequence.<| acc
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
