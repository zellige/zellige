{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as Aeson
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.List                       as DL
import qualified Data.STRef                      as ST
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as VectorStorable
import qualified Geography.VectorTile            as GeographyVectorTile

import qualified Data.Geometry.SphericalMercator as SphericalMercator
import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.Geography   as TypesGeography
import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures
import qualified Data.Geometry.Types.Simplify    as TypesSimplify

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesConfig.ZoomConfig -> [Geospatial.GeoFeature Aeson.Value] -> ST.ST s TypesMvtFeatures.MvtFeatures
geoJsonFeaturesToMvtFeatures zConfig features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature zConfig ops) features

-- Feature

convertFeature :: TypesConfig.ZoomConfig -> ST.STRef s Word -> Geospatial.GeoFeature Aeson.Value -> ST.ST s TypesMvtFeatures.MvtFeatures
convertFeature zConfig ops (Geospatial.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry zConfig fid props geom

-- Geometry

convertGeometry :: TypesConfig.ZoomConfig -> Word -> Aeson.Value -> Geospatial.GeospatialGeometry -> TypesMvtFeatures.MvtFeatures
convertGeometry zConfig fid props geom =
  case geom of
    Geospatial.NoGeometry     -> mempty
    Geospatial.Point g        -> TypesMvtFeatures.mkPoint fid props . convertPoint zConfig $ g
    Geospatial.MultiPoint g   -> TypesMvtFeatures.mkPoint fid props . convertMultiPoint zConfig $ g
    Geospatial.Line g         -> TypesMvtFeatures.mkLineString fid props . convertLineString zConfig $ g
    Geospatial.MultiLine g    -> TypesMvtFeatures.mkLineString fid props . convertMultiLineString zConfig $ g
    Geospatial.Polygon g      -> TypesMvtFeatures.mkPolygon fid props . convertPolygon zConfig $ g
    Geospatial.MultiPolygon g -> TypesMvtFeatures.mkPolygon fid props . convertMultiPolygon zConfig $ g
    Geospatial.Collection gs  -> F.foldMap (convertGeometry zConfig fid props) gs

-- FeatureID

readFeatureID :: Maybe Geospatial.FeatureID -> Maybe Word
readFeatureID mfid =
  case mfid of
    Just (Geospatial.FeatureIDNumber x) -> Just (fromIntegral x)
    _                                   -> Nothing

convertId :: Maybe Geospatial.FeatureID -> ST.STRef s Word -> ST.ST s Word
convertId mfid ops =
  case readFeatureID mfid of
    Just val -> pure val
    Nothing  -> do
      ST.modifySTRef ops (+1)
      ST.readSTRef ops

-- Points

convertPoint :: TypesConfig.ZoomConfig -> Geospatial.GeoPoint -> VectorStorable.Vector GeographyVectorTile.Point
convertPoint zConfig = coordsToPoints zConfig . Geospatial._unGeoPoint

convertMultiPoint :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiPoint -> VectorStorable.Vector GeographyVectorTile.Point
convertMultiPoint zConfig = F.foldMap (convertPoint zConfig) . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: TypesConfig.ZoomConfig -> Geospatial.GeoLine -> Vector.Vector GeographyVectorTile.LineString
convertLineString zConfig@TypesConfig.ZoomConfig{..} =
  Vector.singleton .
  GeographyVectorTile.LineString .
  TypesSimplify.simplifyUsing _zcSimplify .
  Vector.convert .
  F.foldMap (coordsToPoints zConfig) .
  LineString.fromLineString .
  Geospatial._unGeoLine

convertMultiLineString :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiLine -> Vector.Vector GeographyVectorTile.LineString
convertMultiLineString zConfig = F.foldMap (convertLineString zConfig) . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: TypesConfig.ZoomConfig -> Geospatial.GeoPolygon -> Vector.Vector GeographyVectorTile.Polygon
convertPolygon zConfig poly =
  Vector.singleton $
  if Vector.null rawPoly
    then GeographyVectorTile.Polygon mempty mempty
    else
      if Vector.length rawPoly == 1
        then mkPoly zConfig (Vector.head rawPoly)
        else GeographyVectorTile.Polygon (mkPolyPoints zConfig (Vector.head rawPoly)) (mkPolys zConfig (Vector.tail rawPoly))
  where
    rawPoly = Geospatial._unGeoPolygon poly

mkPolys :: Foldable t => TypesConfig.ZoomConfig -> t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Vector.Vector GeographyVectorTile.Polygon
mkPolys zConfig = DL.foldl' (\acc lring -> (mkPoly zConfig lring `Vector.cons` acc)) Vector.empty

mkPoly :: TypesConfig.ZoomConfig -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> GeographyVectorTile.Polygon
mkPoly zConfig lring = GeographyVectorTile.Polygon (mkPolyPoints zConfig lring) mempty

mkPolyPoints :: TypesConfig.ZoomConfig -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector GeographyVectorTile.Point
mkPolyPoints zConfig@TypesConfig.ZoomConfig{..} = TypesSimplify.simplifyUsing _zcSimplify . Vector.convert . F.foldMap (coordsToPoints zConfig)

convertMultiPolygon :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiPolygon -> Vector.Vector GeographyVectorTile.Polygon
convertMultiPolygon zConfig = F.foldMap (convertPolygon zConfig) . Geospatial.splitGeoMultiPolygon

-- Helpers

mkSegments :: VectorStorable.Vector Double -> VectorStorable.Vector (Double, Double)
mkSegments = VectorStorable.zipWith (\a b -> (a, b)) <*> VectorStorable.tail

coordsToPoints :: TypesConfig.ZoomConfig -> Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector GeographyVectorTile.Point
coordsToPoints (TypesConfig.ZoomConfig ext q bb _) coords = VectorStorable.map (SphericalMercator.latLonToXYInTile ext q bb . uncurry TypesGeography.LatLon) (mkSegments $ Geospatial.unGeoPosition coords)

