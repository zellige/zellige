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

import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures
import qualified Data.Geometry.Types.Simplify    as TypesSimplify

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesMvtFeatures.MvtFeatures -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> MonadST.ST s TypesMvtFeatures.MvtFeatures
geoJsonFeaturesToMvtFeatures layer features = do
  ops <- STRef.newSTRef 0
  Foldable.foldMap (convertFeature layer ops) features

-- Feature

convertFeature :: TypesMvtFeatures.MvtFeatures -> STRef.STRef s Word -> Geospatial.GeoFeature Aeson.Value -> MonadST.ST s TypesMvtFeatures.MvtFeatures
convertFeature layer ops (Geospatial.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry layer fid props geom

-- Geometry

convertGeometry :: TypesMvtFeatures.MvtFeatures -> Word -> Aeson.Value -> Geospatial.GeospatialGeometry -> TypesMvtFeatures.MvtFeatures
convertGeometry layer@TypesMvtFeatures.MvtFeatures{..} fid props geom =
  case geom of
    Geospatial.NoGeometry     -> mempty
    Geospatial.Point g        -> layer { TypesMvtFeatures.mvtPoints = TypesMvtFeatures.mkPoint fid props (convertPoint g) mvtPoints }
    Geospatial.MultiPoint g   -> layer { TypesMvtFeatures.mvtPoints = TypesMvtFeatures.mkPoint fid props (convertMultiPoint g) mvtPoints }
    Geospatial.Line g         -> layer { TypesMvtFeatures.mvtLines = TypesMvtFeatures.mkLineString fid props (convertLineString g) mvtLines }
    Geospatial.MultiLine g    -> layer { TypesMvtFeatures.mvtLines = TypesMvtFeatures.mkLineString fid props (convertMultiLineString g) mvtLines }
    Geospatial.Polygon g      -> layer { TypesMvtFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (convertPolygon g) mvtPolygons }
    Geospatial.MultiPolygon g -> layer { TypesMvtFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (convertMultiPolygon g) mvtPolygons }
    Geospatial.Collection gs  -> Foldable.foldMap (convertGeometry layer fid props) gs

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

convertPoint :: Geospatial.GeoPoint -> VectorStorable.Vector VectorTile.Point
convertPoint = coordsToPoints . Geospatial._unGeoPoint

convertMultiPoint :: Geospatial.GeoMultiPoint -> VectorStorable.Vector VectorTile.Point
convertMultiPoint = Foldable.foldMap convertPoint . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: Geospatial.GeoLine -> Vector.Vector VectorTile.LineString
convertLineString =
  Vector.singleton .
  VectorTile.LineString .
  Vector.convert .
  Foldable.foldMap coordsToPoints .
  LineString.fromLineString .
  Geospatial._unGeoLine

convertMultiLineString :: Geospatial.GeoMultiLine -> Vector.Vector VectorTile.LineString
convertMultiLineString = Foldable.foldMap convertLineString . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: Geospatial.GeoPolygon -> Vector.Vector VectorTile.Polygon
convertPolygon poly =
  Vector.singleton $
  if Vector.null rawPoly
    then VectorTile.Polygon mempty mempty
    else
      if Vector.length rawPoly == 1
        then mkPoly (Vector.head rawPoly)
        else VectorTile.Polygon (mkPolyPoints (Vector.head rawPoly)) (mkPolys (Vector.tail rawPoly))
  where
    rawPoly = Geospatial._unGeoPolygon poly

mkPolys :: Foldable t => t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Vector.Vector VectorTile.Polygon
mkPolys = List.foldl' (\acc lring -> (mkPoly lring `Vector.cons` acc)) Vector.empty

mkPoly :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly lring = VectorTile.Polygon (mkPolyPoints lring) mempty

mkPolyPoints :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector VectorTile.Point
mkPolyPoints = Vector.convert . Foldable.foldMap coordsToPoints

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Vector.Vector VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector VectorTile.Point
coordsToPoints _ = undefined

