{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonQuickClip (
clipPolygonQc
, clipPolygonsQc
) where

import qualified Data.Aeson                                as Aeson
import qualified Data.Foldable                             as Foldable
import qualified Data.Geometry.Clip.Internal.Line          as ClipLine
import qualified Data.Geometry.Clip.Internal.LineQuickClip as ClipLineQuickClip
import qualified Data.Geometry.Types.Geography             as TypesGeography
import qualified Data.Geospatial                           as Geospatial
import qualified Data.LinearRing                           as LinearRing
import qualified Data.List.NonEmpty                        as ListNonEmpty
import qualified Data.Sequence                             as Sequence
import qualified Data.Validation                           as Validation

clipPolygonsQc :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygonsQc bb (Geospatial.GeoMultiPolygon polys) (Geospatial.GeoFeature bbox _ props fId) acc =
  case maybeNewMultiPoly bb polys of
    Nothing   -> acc
    Just newPolys -> (Sequence.<|) (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon newPolys)) props fId) acc

maybeNewMultiPoly :: TypesGeography.BoundingBox -> Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
maybeNewMultiPoly bb = traverse $ traverse (clipLinearRing bb)

clipPolygonQc :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPolygonQc bb (Geospatial.GeoPolygon poly) (Geospatial.GeoFeature bbox _ props fId) acc =
    case maybeNewPoly bb poly of
      Nothing   -> acc
      Just newPoly -> (Sequence.<|) (Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon newPoly)) props fId) acc

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
clipPolyPoints bb polyPoints =
  if Sequence.length newClippedPoly <= 2
  then Nothing
  else Just (closeIfNot newClippedPoly)
  where
    newClippedPoly = foo bb polyPoints

closeIfNot :: Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
closeIfNot poly@(firstPt Sequence.:<| (_ Sequence.:|> lastPt)) =
  if lastPt /= firstPt
    then lastPt Sequence.<| poly
    else poly


foo ::  TypesGeography.BoundingBox-> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
foo bb polyPts = if Sequence.length polyPts <= 2 then Sequence.empty else newPoints
  where
    newPoints = lineToClippedPoints bb (TypesGeography.pointsToLines polyPts)

lineToClippedPoints :: TypesGeography.BoundingBox -> Sequence.Seq TypesGeography.GeoStorableLine -> Sequence.Seq Geospatial.PointXY
lineToClippedPoints bb l = ClipLine.lineToPointXY $ Foldable.foldr (ClipLineQuickClip.clipOrDiscard bb) Sequence.empty
