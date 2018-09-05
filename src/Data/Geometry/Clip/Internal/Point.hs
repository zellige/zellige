module Data.Geometry.Clip.Internal.Point (
 clipPoints
, clipPoint
) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Vector                   as Vector

import qualified Data.Geometry.Types.Geography as TypesGeography

clipPoint :: TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Vector.Vector Geospatial.GeoPoint
clipPoint bbox point = if pointInsideExtent bbox point then Vector.singleton point else Vector.empty

clipPoints :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPoint -> Vector.Vector Geospatial.GeoPoint
clipPoints bbox mp = Vector.filter (pointInsideExtent bbox) (Geospatial.splitGeoMultiPoint mp)

pointInsideExtent :: TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Bool
pointInsideExtent (TypesGeography.BoundingBox minX minY maxX maxY) point = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (x, y) = case Geospatial._unGeoPoint point of
      (Geospatial.GeoPointXY (Geospatial.PointXY pX pY))         -> (pX, pY)
      (Geospatial.GeoPointXYZ (Geospatial.PointXYZ pX pY _))     -> (pX, pY)
      (Geospatial.GeoPointXYZM (Geospatial.PointXYZM pX pY _ _)) -> (pX, pY)
