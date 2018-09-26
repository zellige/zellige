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
import qualified Data.Sequence                   as Sequence
import qualified Data.STRef                      as STRef
import qualified Geography.VectorTile            as VectorTile

import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesMvtFeatures.MvtFeatures -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> MonadST.ST s TypesMvtFeatures.MvtFeatures
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

convertPoint :: Geospatial.GeoPoint -> Sequence.Seq VectorTile.Point
convertPoint = coordsToPoints . Geospatial._unGeoPoint

convertMultiPoint :: Geospatial.GeoMultiPoint -> Sequence.Seq VectorTile.Point
convertMultiPoint = Foldable.foldMap convertPoint . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: Geospatial.GeoLine -> Sequence.Seq VectorTile.LineString
convertLineString =
  Sequence.singleton .
  VectorTile.LineString .
  VectorStorable.uniq .
  Foldable.foldMap coordsToPoints .
  LineString.fromLineString .
  Geospatial._unGeoLine

convertMultiLineString :: Geospatial.GeoMultiLine -> Sequence.Seq VectorTile.LineString
convertMultiLineString = Foldable.foldMap convertLineString . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: Geospatial.GeoPolygon -> Sequence.Seq VectorTile.Polygon
convertPolygon poly =
  Sequence.singleton $
  if Sequence.null rawPoly
    then VectorTile.Polygon mempty mempty
    else
      if Sequence.length rawPoly == 1
        then mkPoly (Vector.head rawPoly)
        else VectorTile.Polygon (mkPolyPoints (Vector.head rawPoly)) (mkPolys (Vector.tail rawPoly))
  where
    rawPoly = Geospatial._unGeoPolygon poly

mkPolys :: Foldable t => t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Sequence.Seq VectorTile.Polygon
mkPolys = List.foldl' (\acc lring -> (mkPoly lring Sequence.<| acc)) Sequence.empty

mkPoly :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly lring = VectorTile.Polygon (mkPolyPoints lring) mempty

mkPolyPoints :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
mkPolyPoints = VectorStorable.uniq . foldMap coordsToPoints

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Sequence.Seq VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
coordsToPoints geoPosition = Sequence.singleton newPoint
    where
      newPoint = VectorTile.Point (round posX) (round posY)
      (Geospatial.PointXY posX posY) = Geospatial.retrieveXY geoPosition

