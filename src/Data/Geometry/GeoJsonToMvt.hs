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
import qualified Data.Geometry.Types.MvtFeatures     as TypesMvtFeatures

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesGeoJsonFeatures.MvtFeatures -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> MonadST.ST s TypesGeoJsonFeatures.MvtFeatures
geoJsonFeaturesToMvtFeatures layer features = do
  ops <- STRef.newSTRef 0
  Foldable.foldMap (convertFeature layer ops) features

-- Feature

convertFeature :: TypesGeoJsonFeatures.MvtFeatures -> STRef.STRef s Word -> Geospatial.GeoFeature Aeson.Value -> MonadST.ST s TypesGeoJsonFeatures.MvtFeatures
convertFeature layer ops (Geospatial.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry layer fid props geom

-- Geometry

convertGeometry :: TypesGeoJsonFeatures.MvtFeatures -> Word -> Aeson.Value -> Geospatial.GeospatialGeometry -> TypesGeoJsonFeatures.MvtFeatures
convertGeometry layer@TypesGeoJsonFeatures.MvtFeatures{..} fid props geom =
  case geom of
    Geospatial.NoGeometry     -> mempty
    Geospatial.Point g        -> layer { TypesGeoJsonFeatures.mvtPoints   = TypesMvtFeatures.mkPoint fid props (TypesGeoJsonFeatures.convertPoint g) mvtPoints }
    Geospatial.MultiPoint g   -> layer { TypesGeoJsonFeatures.mvtPoints   = TypesMvtFeatures.mkPoint fid props (TypesGeoJsonFeatures.convertMultiPoint g) mvtPoints }
    Geospatial.Line g         -> layer { TypesGeoJsonFeatures.mvtLines    = TypesMvtFeatures.mkLineString fid props (TypesGeoJsonFeatures.convertLineString g) mvtLines }
    Geospatial.MultiLine g    -> layer { TypesGeoJsonFeatures.mvtLines    = TypesMvtFeatures.mkLineString fid props (TypesGeoJsonFeatures.convertMultiLineString g) mvtLines }
    Geospatial.Polygon g      -> layer { TypesGeoJsonFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (TypesGeoJsonFeatures.convertPolygon g) mvtPolygons }
    Geospatial.MultiPolygon g -> layer { TypesGeoJsonFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (TypesGeoJsonFeatures.convertMultiPolygon g) mvtPolygons }
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
