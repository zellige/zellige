{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonStreamingToMvt where

import qualified Control.Foldl                                                     as Foldl
import qualified Data.Aeson.Types                                                  as AesonTypes
import qualified Data.ByteString                                                   as ByteString
import qualified Data.ByteString.Lazy                                              as ByteStringLazy
import qualified Data.Geometry.VectorTile.Protobuf.Internal.Vector_tile.Tile       as Tile
import qualified Data.Geometry.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Layer
import qualified Data.Geospatial                                                   as Geospatial
import qualified Data.HashMap.Strict                                               as HashMapStrict
import qualified Data.Sequence                                                     as Sequence
import           Prelude                                                           hiding
                                                                                    (Left,
                                                                                    Right)
import qualified Text.ProtocolBuffers.Basic                                        as ProtocolBuffersBasic
import qualified Text.ProtocolBuffers.WireMessage                                  as WireMessage

import qualified Data.Geometry.Types.Config                                        as TypesConfig
import qualified Data.Geometry.Types.GeoJsonFeatures                               as TypesGeoJsonFeatures
import qualified Data.Geometry.Types.MvtFeatures                                   as TypesMvtFeatures


foldStreamingLayer :: Foldl.Fold (Geospatial.GeospatialGeometry, AesonTypes.Value) TypesMvtFeatures.StreamingLayer
foldStreamingLayer = foldStreamingLayerFrom (Just 0)

foldStreamingLayerFrom :: Maybe Word -> Foldl.Fold (Geospatial.GeospatialGeometry, AesonTypes.Value) TypesMvtFeatures.StreamingLayer
foldStreamingLayerFrom maybeStartId = Foldl.Fold step begin done
  where
    begin = TypesMvtFeatures.emptyStreamingLayer maybeStartId

    step (TypesMvtFeatures.StreamingLayer featureId ks vs features) (geom, value) = TypesMvtFeatures.StreamingLayer incFeatureId (TypesMvtFeatures.KeyStore newKeyCount newKeyStore newKeyList) (TypesMvtFeatures.ValueStore newValueCount newValueStore newValueList) newFeatures
      where
        incFeatureId = fmap (+1) featureId
        convertedProps = TypesGeoJsonFeatures.convertProps value
        (newKeyCount, newKeyStore, newKeyList) = TypesMvtFeatures.newKeys ks (HashMapStrict.keys convertedProps)
        (newValueCount, newValueStore, newValueList) = TypesMvtFeatures.newValues vs (HashMapStrict.elems convertedProps)
        newFeatures = TypesMvtFeatures.newConvertGeometry features incFeatureId convertedProps newKeyStore newValueStore geom

    done = id

createLayerFromStreamingLayer :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> Layer.Layer
createLayerFromStreamingLayer TypesConfig.Config{..} (TypesMvtFeatures.StreamingLayer _ (TypesMvtFeatures.KeyStore _ _ keysList) (TypesMvtFeatures.ValueStore _ _ valuesList) features) = Layer.Layer
  { Layer.version   = fromIntegral _version
  , Layer.name      = ProtocolBuffersBasic.Utf8 _name
  , Layer.features  = features
  , Layer.keys      = keysList
  , Layer.values    = valuesList
  , Layer.extent    = fmap fromIntegral _extents
  , Layer.ext'field = ProtocolBuffersBasic.defaultValue
  }

createTileFromStreamingLayer :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> Tile.Tile
createTileFromStreamingLayer config sl@TypesMvtFeatures.StreamingLayer{..}
  | Sequence.null slFeatures = Tile.Tile
    { Tile.layers = Sequence.empty
    , Tile.ext'field = ProtocolBuffersBasic.defaultValue
    }
  | otherwise = Tile.Tile
    { Tile.layers    = Sequence.singleton (createLayerFromStreamingLayer config sl)
    , Tile.ext'field = ProtocolBuffersBasic.defaultValue
    }

vtToBytes :: TypesConfig.Config -> TypesMvtFeatures.StreamingLayer -> ByteString.ByteString
vtToBytes config sl = ByteStringLazy.toStrict . WireMessage.messagePut $ createTileFromStreamingLayer config sl

