{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                    as MonadST
import qualified Data.Aeson                          as Aeson
import qualified Data.Foldable                       as Foldable
import qualified Data.Geospatial                     as Geospatial
import           Data.Monoid
import qualified Data.Sequence                       as Sequence
import qualified Data.STRef                          as STRef
import           Prelude                             hiding (Left, Right)

import qualified Data.Geometry.Types.GeoJsonFeatures as TypesGeoJsonFeatures
import qualified Data.Geometry.VectorTile.Types      as VectorTileTypes

-- Lib

geoJsonFeaturesToMvtFeatures :: VectorTileTypes.MvtFeatures -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> MonadST.ST s VectorTileTypes.MvtFeatures
geoJsonFeaturesToMvtFeatures layer features = do
  ops <- STRef.newSTRef 0
  Foldable.foldMap (convertFeature layer ops) features

-- Feature

convertFeature :: VectorTileTypes.MvtFeatures -> STRef.STRef s Word -> Geospatial.GeoFeature Aeson.Value -> MonadST.ST s VectorTileTypes.MvtFeatures
convertFeature layer ops (Geospatial.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry layer fid props geom

-- Geometry

convertGeometry :: VectorTileTypes.MvtFeatures -> Word -> Aeson.Value -> Geospatial.GeospatialGeometry -> VectorTileTypes.MvtFeatures
convertGeometry layer@VectorTileTypes.MvtFeatures{..} fid props geom =
  case geom of
    Geospatial.NoGeometry     -> mempty
    Geospatial.Point g        -> layer { VectorTileTypes.mvtPoints   = TypesGeoJsonFeatures.mkPoint fid props (TypesGeoJsonFeatures.convertPoint g) mvtPoints }
    Geospatial.MultiPoint g   -> layer { VectorTileTypes.mvtPoints   = TypesGeoJsonFeatures.mkPoint fid props (TypesGeoJsonFeatures.convertMultiPoint g) mvtPoints }
    Geospatial.Line g         -> layer { VectorTileTypes.mvtLines    = TypesGeoJsonFeatures.mkLineString fid props (TypesGeoJsonFeatures.convertLineString g) mvtLines }
    Geospatial.MultiLine g    -> layer { VectorTileTypes.mvtLines    = TypesGeoJsonFeatures.mkLineString fid props (TypesGeoJsonFeatures.convertMultiLineString g) mvtLines }
    Geospatial.Polygon g      -> layer { VectorTileTypes.mvtPolygons = TypesGeoJsonFeatures.mkPolygon fid props (TypesGeoJsonFeatures.convertPolygon g) mvtPolygons }
    Geospatial.MultiPolygon g -> layer { VectorTileTypes.mvtPolygons = TypesGeoJsonFeatures.mkPolygon fid props (TypesGeoJsonFeatures.convertMultiPolygon g) mvtPolygons }
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
