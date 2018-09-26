module Data.Geometry.Clip.Internal.Point (
 clipPoints
, clipPoint
, clipPointMap
, clipPointsMap
) where

import           Control.Applicative
import qualified Data.Aeson                    as Aeson
import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geospatial               as Geospatial
import qualified Data.Sequence                 as Sequence

clipPoint :: TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPoint bbox point feature acc =
  if pointInsideExtent bbox (Geospatial._unGeoPoint point)
    then (Sequence.<|) feature acc
    else acc

clipPointMap :: TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Maybe Geospatial.GeoPoint
clipPointMap bbox point = if pointInsideExtent bbox (Geospatial._unGeoPoint point) then Just point else Nothing

clipPoints :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPoint -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipPoints bbox multiPoint feature acc =
  case clipPointsMap bbox multiPoint of
    Nothing -> acc
    Just newPoints ->(Sequence.<|) (feature { Geospatial._geometry = Geospatial.MultiPoint newPoints }) acc

clipPointsMap :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPoint -> Maybe Geospatial.GeoMultiPoint
clipPointsMap bbox multiPoint = if Sequence.null newPoints then Nothing else Just (Geospatial.GeoMultiPoint newPoints)
  where
    newPoints = Sequence.filter (pointInsideExtent bbox) (Geospatial._unGeoPoint Control.Applicative.<$> Geospatial.splitGeoMultiPoint multiPoint)


pointInsideExtent :: TypesGeography.BoundingBox -> Geospatial.GeoPositionWithoutCRS -> Bool
pointInsideExtent (TypesGeography.BoundingBox minX minY maxX maxY) position =
  case position of
    Geospatial.GeoEmpty                                        -> False
    (Geospatial.GeoPointXY (Geospatial.PointXY pX pY))         -> comparePt pX pY
    (Geospatial.GeoPointXYZ (Geospatial.PointXYZ pX pY _))     -> comparePt pX pY
    (Geospatial.GeoPointXYZM (Geospatial.PointXYZM pX pY _ _)) -> comparePt pX pY
  where
    comparePt x y = x >= minX && x <= maxX && y >= minY && y <= maxY
