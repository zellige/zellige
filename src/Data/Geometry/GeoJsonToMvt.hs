{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as MonadST
import qualified Data.Aeson                      as Aeson
import qualified Data.Foldable                   as Foldable
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.List                       as List
import qualified Data.STRef                      as STRef
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as VectorStorable
import qualified Geography.VectorTile            as VectorTile

import qualified Data.Geometry.SphericalMercator as SphericalMercator
import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.Geography   as TypesGeography
import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures
import qualified Data.Geometry.Types.Simplify    as TypesSimplify

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesConfig.ZoomConfig -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> MonadST.ST s TypesMvtFeatures.MvtFeatures
geoJsonFeaturesToMvtFeatures zConfig features = do
  ops <- STRef.newSTRef 0
  Foldable.foldMap (convertFeature zConfig ops) features

-- Feature

convertFeature :: TypesConfig.ZoomConfig -> STRef.STRef s Word -> Geospatial.GeoFeature Aeson.Value -> MonadST.ST s TypesMvtFeatures.MvtFeatures
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
    Geospatial.Collection gs  -> Foldable.foldMap (convertGeometry zConfig fid props) gs

-- FeatureID

readFeatureID :: Maybe Geospatial.FeatureID -> Maybe Word
readFeatureID mfid =
  case mfid of
    Just (Geospatial.FeatureIDNumber x) -> Just (fromIntegral x)
    _                                   -> Nothing

convertId :: Maybe Geospatial.FeatureID -> STRef.STRef s Word -> MonadST.ST s Word
convertId mfid ops =
  case readFeatureID mfid of
    Just val -> pure val
    Nothing  -> do
      STRef.modifySTRef ops (+1)
      STRef.readSTRef ops

-- Points

convertPoint :: TypesConfig.ZoomConfig -> Geospatial.GeoPoint -> VectorStorable.Vector VectorTile.Point
convertPoint zConfig = coordsToPoints zConfig . Geospatial._unGeoPoint

convertMultiPoint :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiPoint -> VectorStorable.Vector VectorTile.Point
convertMultiPoint zConfig = Foldable.foldMap (convertPoint zConfig) . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: TypesConfig.ZoomConfig -> Geospatial.GeoLine -> Vector.Vector VectorTile.LineString
convertLineString zConfig@TypesConfig.ZoomConfig{..} =
  Vector.singleton .
  VectorTile.LineString .
  Vector.convert .
  Foldable.foldMap (coordsToPoints zConfig) .
  LineString.fromLineString .
  Geospatial._unGeoLine

convertMultiLineString :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiLine -> Vector.Vector VectorTile.LineString
convertMultiLineString zConfig = Foldable.foldMap (convertLineString zConfig) . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: TypesConfig.ZoomConfig -> Geospatial.GeoPolygon -> Vector.Vector VectorTile.Polygon
convertPolygon zConfig poly =
  Vector.singleton $
  if Vector.null rawPoly
    then VectorTile.Polygon mempty mempty
    else
      if Vector.length rawPoly == 1
        then mkPoly zConfig (Vector.head rawPoly)
        else VectorTile.Polygon (mkPolyPoints zConfig (Vector.head rawPoly)) (mkPolys zConfig (Vector.tail rawPoly))
  where
    rawPoly = Geospatial._unGeoPolygon poly

mkPolys :: Foldable t => TypesConfig.ZoomConfig -> t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Vector.Vector VectorTile.Polygon
mkPolys zConfig = List.foldl' (\acc lring -> (mkPoly zConfig lring `Vector.cons` acc)) Vector.empty

mkPoly :: TypesConfig.ZoomConfig -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly zConfig lring = VectorTile.Polygon (mkPolyPoints zConfig lring) mempty

mkPolyPoints :: TypesConfig.ZoomConfig -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector VectorTile.Point
mkPolyPoints zConfig@TypesConfig.ZoomConfig{..} = TypesSimplify.simplifyUsing _zcSimplify . Vector.convert . Foldable.foldMap (coordsToPoints zConfig)

convertMultiPolygon :: TypesConfig.ZoomConfig -> Geospatial.GeoMultiPolygon -> Vector.Vector VectorTile.Polygon
convertMultiPolygon zConfig = Foldable.foldMap (convertPolygon zConfig) . Geospatial.splitGeoMultiPolygon

-- Helpers

mkSegments :: VectorStorable.Vector Double -> VectorStorable.Vector (Double, Double)
mkSegments = VectorStorable.zipWith (\a b -> (a, b)) <*> VectorStorable.tail

coordsToPoints :: TypesConfig.ZoomConfig -> Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector VectorTile.Point
coordsToPoints (TypesConfig.ZoomConfig ext _ bb _) coords = VectorStorable.map (SphericalMercator.latLonToXYInTile ext bb . uncurry TypesGeography.LatLon) (mkSegments $ Geospatial.unGeoPosition coords)

