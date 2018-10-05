{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonStreamingToMvt where

import qualified Control.Foldl                                                 as Foldl
import qualified Data.Aeson.Types                                              as AesonTypes
import qualified Data.ByteString                                               as ByteString
import qualified Data.ByteString.Lazy                                          as ByteStringLazy
import qualified Data.Geospatial                                               as Geospatial
import qualified Data.HashMap.Strict                                           as HashMapStrict
import           Data.Monoid
import qualified Data.Sequence                                                 as Sequence
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile       as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Layer
import           Prelude                                                       hiding
                                                                                (Left,
                                                                                Right)
import qualified Text.ProtocolBuffers.Basic                                    as ProtocolBuffersBasic
import qualified Text.ProtocolBuffers.WireMessage                              as WireMessage

import qualified Data.Geometry.Types.Config                                    as TypesConfig
import qualified Data.Geometry.Types.MvtFeatures                               as TypesMvtFeatures

foldStreamingLayer :: Foldl.Fold (Geospatial.GeospatialGeometry, AesonTypes.Value) TypesMvtFeatures.StreamingLayer
foldStreamingLayer = Foldl.Fold step begin done
  where
    begin = TypesMvtFeatures.StreamingLayer 1 (TypesMvtFeatures.KeyStore 0 mempty mempty) (TypesMvtFeatures.ValueStore 0 mempty mempty) mempty

    step (TypesMvtFeatures.StreamingLayer featureId ks vs features) (geom, value) = TypesMvtFeatures.StreamingLayer (featureId + 1) (TypesMvtFeatures.KeyStore newKeyCount newKeyStore newKeyList) (TypesMvtFeatures.ValueStore newValueCount newValueStore newValueList) newFeatures
      where
        convertedProps = TypesMvtFeatures.convertProps value
        (newKeyCount, newKeyStore, newKeyList) = TypesMvtFeatures.newKeys ks (HashMapStrict.keys convertedProps)
        (newValueCount, newValueStore, newValueList) = TypesMvtFeatures.newValues vs (HashMapStrict.elems convertedProps)
        newFeatures = TypesMvtFeatures.newConvertGeometry features featureId convertedProps newKeyStore newValueStore geom

    done = id

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

