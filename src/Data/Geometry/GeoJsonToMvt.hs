{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                                              as MonadST
import qualified Data.Aeson                                                    as Aeson
import qualified Data.ByteString                                               as ByteString
import qualified Data.ByteString.Lazy                                          as ByteStringLazy
import qualified Data.Foldable                                                 as Foldable
import qualified Data.Geospatial                                               as Geospatial
import qualified Data.Sequence                                                 as Sequence
import qualified Data.STRef                                                    as STRef
import qualified Data.Vector                                                   as Vector
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile       as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Layer
import qualified Text.ProtocolBuffers.Basic                                    as ProtocolBuffersBasic
import qualified Text.ProtocolBuffers.WireMessage                              as WireMessage

import qualified Data.Geometry.Types.Config                                    as TypesConfig
import qualified Data.Geometry.Types.MvtFeatures                               as TypesMvtFeatures

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

createLayerFromStreamingLayer :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> Layer.Layer
createLayerFromStreamingLayer TypesConfig.Config{..} (TypesMvtFeatures.StreamingLayer _ (TypesMvtFeatures.KeyStore _ _ keysList) (TypesMvtFeatures.ValueStore _ _ valuesList) features) = Layer.Layer
  { Layer.version   = fromIntegral _version
  , Layer.name      = ProtocolBuffersBasic.Utf8 _name
  , Layer.features  = features
  , Layer.keys      = keysList
  , Layer.values    = valuesList
  , Layer.extent    = Just $ fromIntegral _extents
  , Layer.ext'field = ProtocolBuffersBasic.defaultValue
  }

createTileFromStreamingLayer :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> Tile.Tile
createTileFromStreamingLayer config sl = Tile.Tile
  { Tile.layers    = Sequence.fromList [createLayerFromStreamingLayer config sl]
  , Tile.ext'field = ProtocolBuffersBasic.defaultValue
  }

vtToBytes :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> ByteString.ByteString
vtToBytes config sl = ByteStringLazy.toStrict . WireMessage.messagePut $ createTileFromStreamingLayer config sl

-- Geometry

convertGeometry :: TypesMvtFeatures.MvtFeatures -> Word -> Aeson.Value -> Geospatial.GeospatialGeometry -> TypesMvtFeatures.MvtFeatures
convertGeometry layer@TypesMvtFeatures.MvtFeatures{..} fid props geom =
  case geom of
    Geospatial.NoGeometry     -> mempty
    Geospatial.Point g        -> layer { TypesMvtFeatures.mvtPoints = TypesMvtFeatures.mkPoint fid props (TypesMvtFeatures.convertPoint g) mvtPoints }
    Geospatial.MultiPoint g   -> layer { TypesMvtFeatures.mvtPoints = TypesMvtFeatures.mkPoint fid props (TypesMvtFeatures.convertMultiPoint g) mvtPoints }
    Geospatial.Line g         -> layer { TypesMvtFeatures.mvtLines = TypesMvtFeatures.mkLineString fid props (TypesMvtFeatures.convertLineString g) mvtLines }
    Geospatial.MultiLine g    -> layer { TypesMvtFeatures.mvtLines = TypesMvtFeatures.mkLineString fid props (TypesMvtFeatures.convertMultiLineString g) mvtLines }
    Geospatial.Polygon g      -> layer { TypesMvtFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (TypesMvtFeatures.convertPolygon g) mvtPolygons }
    Geospatial.MultiPolygon g -> layer { TypesMvtFeatures.mvtPolygons = TypesMvtFeatures.mkPolygon fid props (TypesMvtFeatures.convertMultiPolygon g) mvtPolygons }
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
