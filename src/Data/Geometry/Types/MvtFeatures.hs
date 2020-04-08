{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Data.ByteString.Lazy                                                 as ByteStringLazy
import qualified Data.Foldable                                                        as Foldable
import qualified Data.Geometry.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature  as Feature
import qualified Data.Geometry.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import qualified Data.Geometry.VectorTile.VectorTile                                  as VectorTile
import qualified Data.Geospatial                                                      as Geospatial
import qualified Data.Hashable                                                        as Hashable
import qualified Data.HashMap.Strict                                                  as HashMapStrict
import qualified Data.Sequence                                                        as Sequence
import           Prelude                                                              hiding
                                                                                       (Left,
                                                                                       Right)
import qualified Text.ProtocolBuffers.Basic                                           as ProtocolBuffersBasic

import qualified Data.Geometry.Types.GeoJsonFeatures                                  as TypesGeoJsonFeatures

-- Fold (x -> a -> x) x (x -> b) -- Fold step initial extract
data StreamingLayer = StreamingLayer
  { featureId    :: Word
  , slKeyStore   :: KeyStore
  , slValueStore :: ValueStore
  , slFeatures   :: Sequence.Seq Feature.Feature
  }

emptyStreamingLayer :: StreamingLayer
emptyStreamingLayer = StreamingLayer 1 (KeyStore 0 mempty mempty) (ValueStore 0 mempty mempty) mempty

data KeyStore = KeyStore
  { ksKeyInt :: Int
  , ksKeys   :: HashMapStrict.HashMap ByteStringLazy.ByteString Int
  , ksList   :: Sequence.Seq ProtocolBuffersBasic.Utf8
  }

data ValueStore = ValueStore
  { vsValueInt :: Int
  , vsValues   :: HashMapStrict.HashMap VectorTile.Val Int
  , vsList     :: Sequence.Seq (VectorTile.Protobuf VectorTile.Val)
  }

newKeys :: Foldable t => KeyStore -> t ProtocolBuffersBasic.ByteString -> (Int, HashMapStrict.HashMap ProtocolBuffersBasic.ByteString Int, ProtocolBuffersBasic.Seq ProtocolBuffersBasic.Utf8)
newKeys (KeyStore keyCount keyMap keyList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq ProtocolBuffersBasic.Utf8) (keyCount, keyMap, keyList)

newValues :: Foldable t => ValueStore -> t VectorTile.Val -> (Int, HashMapStrict.HashMap VectorTile.Val Int, ProtocolBuffersBasic.Seq VectorTile.Value)
newValues (ValueStore valueCount valueMap valueList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq VectorTile.toProtobuf) (valueCount, valueMap, valueList)

addKeyValue :: (Eq a, Hashable.Hashable a) => Int -> a -> HashMapStrict.HashMap a Int -> Sequence.Seq b -> (a -> b)-> (Int, HashMapStrict.HashMap a Int, Sequence.Seq b)
addKeyValue currentKeyNumber key hashMap seqs f =
  case HashMapStrict.lookup key hashMap of
    Nothing -> (currentKeyNumber + 1, HashMapStrict.insert key currentKeyNumber hashMap, seqs Sequence.|> f key)
    Just _  -> (currentKeyNumber, hashMap, seqs)

newConvertGeometry :: Sequence.Seq Feature.Feature -> Word -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val -> HashMapStrict.HashMap ByteStringLazy.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> Geospatial.GeospatialGeometry -> ProtocolBuffersBasic.Seq Feature.Feature
newConvertGeometry acc fid convertedProps keys values geom =
  case geom of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> checkAndAdd keys values (Just GeomType.POINT) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertPoint g)) acc
    Geospatial.MultiPoint g   -> checkAndAdd keys values (Just GeomType.POINT) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiPoint g)) acc
    Geospatial.Line g         -> checkAndAdd keys values (Just GeomType.LINESTRING) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertLineString g)) acc
    Geospatial.MultiLine g    -> checkAndAdd keys values (Just GeomType.LINESTRING) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiLineString g)) acc
    Geospatial.Polygon g      -> checkAndAdd keys values (Just GeomType.POLYGON) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertPolygon g)) acc
    Geospatial.MultiPolygon g -> checkAndAdd keys values (Just GeomType.POLYGON) (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiPolygon g)) acc
    Geospatial.Collection gs -> Foldable.foldMap (newConvertGeometry acc fid convertedProps keys values) gs

checkAndAdd :: (VectorTile.ProtobufGeom g, VectorTile.GeomVec g ~ ProtocolBuffersBasic.Seq a) => HashMapStrict.HashMap ProtocolBuffersBasic.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> Maybe GeomType.GeomType -> VectorTile.Feature (ProtocolBuffersBasic.Seq a) -> ProtocolBuffersBasic.Seq Feature.Feature -> ProtocolBuffersBasic.Seq Feature.Feature
checkAndAdd keys values featureType feature@(VectorTile.Feature _ _ geoms) acc =
  if Sequence.null geoms
    then acc
    else VectorTile.unfeats keys values featureType feature Sequence.<| acc
