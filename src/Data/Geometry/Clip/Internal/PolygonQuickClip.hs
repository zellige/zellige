{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonQuickClip (
clipPolygonQc
, clipPolygonsQc
) where

import qualified Data.Aeson                                as Aeson
import qualified Data.Geometry.Clip.Internal.Line          as ClipLine
import qualified Data.Geometry.Clip.Internal.LineQuickClip as ClipLineQuickClip
import qualified Data.Geometry.Types.Geography             as TypesGeography
import qualified Data.Geospatial                           as Geospatial
import qualified Data.LinearRing                           as LinearRing
import qualified Data.List.NonEmpty                        as ListNonEmpty
import qualified Data.Validation                           as Validation
import qualified Data.Vector                               as Vector
import qualified Data.Vector.Storable                      as VectorStorable

clipPolygonsQc :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygonsQc bb (Geospatial.GeoMultiPolygon polys) (Geospatial.GeoFeature bbox _ props fId) acc =
  case maybeNewMultiPoly bb polys of
    Nothing   -> acc
    Just newPolys -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon newPolys)) props fId) acc

maybeNewMultiPoly :: TypesGeography.BoundingBox -> Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
maybeNewMultiPoly bb = traverse $ traverse (clipLinearRing bb)

clipPolygonQc :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygonQc bb (Geospatial.GeoPolygon poly) (Geospatial.GeoFeature bbox _ props fId) acc =
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
    newClippedPoly = foo bb polyPoints
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

foo ::  TypesGeography.BoundingBox-> Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
foo bb polyPts = if Vector.length polyPts <= 2 then Vector.empty else newPoints
  where
    newPoints = lineToClippedPoints bb (TypesGeography.pointsToLines polyPts)

lineToClippedPoints :: TypesGeography.BoundingBox -> Vector.Vector TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY
lineToClippedPoints bb l = ClipLine.foldPointsToLine . Vector.convert $ VectorStorable.foldr (ClipLineQuickClip.clipOrDiscard bb) VectorStorable.empty (VectorStorable.convert l)
