{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Data.Aeson                                                       as Aeson
import qualified Data.ByteString.Lazy                                             as ByteStringLazy
import qualified Data.Foldable                                                    as Foldable
import qualified Data.Geospatial                                                  as Geospatial
import qualified Data.Hashable                                                    as Hashable
import qualified Data.HashMap.Strict                                              as HashMapStrict
import qualified Data.Scientific                                                  as Scientific
import qualified Data.Sequence                                                    as Sequence
import qualified Data.Text                                                        as Text
import qualified Data.Text.Encoding                                               as TextEncoding
import qualified Data.Geometry.VectorTile.VectorTile as VectorTile
import qualified Data.Geometry.VectorTile.Internal                                    as VectorTileInternal
import qualified Data.Geometry.VectorTile.Geometry as VectorTileGeometry
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature  as Feature
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import           Prelude                                                          hiding
                                                                                   (Left,
                                                                                   Right)
import qualified Text.ProtocolBuffers.Basic                                       as ProtocolBuffersBasic

import qualified Data.Geometry.Types.GeoJsonFeatures                              as TypesGeoJsonFeatures

mkPoint :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.Point -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.Point)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.Point))
mkPoint fId props p = (Sequence.<|) (VectorTile.Feature fId (convertProps props) p)

mkLineString :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.LineString -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.LineString)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.LineString))
mkLineString fId props l = (Sequence.<|) (mkFeature fId props l)

mkPolygon :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.Polygon -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.Polygon)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTileGeometry.Polygon))
mkPolygon x props o = (Sequence.<|) (mkFeature x props o)

mkFeature :: Word -> Aeson.Value -> Sequence.Seq g -> VectorTile.Feature (Sequence.Seq g)
mkFeature fId props = VectorTile.Feature fId (convertProps props)

convertProps :: Aeson.Value -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val
convertProps (Aeson.Object !x) = HashMapStrict.foldrWithKey (\k v acc -> maybe acc (\(!k', !v') -> HashMapStrict.insert k' v' acc) (convertElems (k, v))) HashMapStrict.empty x
convertProps _                 = HashMapStrict.empty

convertElems :: (Text.Text, Aeson.Value) -> Maybe (ByteStringLazy.ByteString, VectorTile.Val)
convertElems (!k, Aeson.String !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.St ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) v))
convertElems (!k, Aeson.Number !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.Do (sToF v))
convertElems (!k, Aeson.Bool !v)   = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.B v)
convertElems _                     = Nothing

sToF :: Scientific.Scientific -> Double
sToF = Scientific.toRealFloat

-- Fold (x -> a -> x) x (x -> b) -- Fold step initial extract
data StreamingLayer = StreamingLayer
  { featureId    :: Word
  , slKeyStore   :: KeyStore
  , slValueStore :: ValueStore
  , slFeatures   :: Sequence.Seq Feature.Feature
  }

data KeyStore = KeyStore
  { ksKeyInt :: Int
  , ksKeys   :: HashMapStrict.HashMap ByteStringLazy.ByteString Int
  , ksList   :: Sequence.Seq ProtocolBuffersBasic.Utf8
  }

data ValueStore = ValueStore
  { vsValueInt :: Int
  , vsValues   :: HashMapStrict.HashMap VectorTile.Val Int
  , vsList     :: Sequence.Seq (VectorTileInternal.Protobuf VectorTile.Val)
  }

newKeys :: Foldable t => KeyStore -> t ProtocolBuffersBasic.ByteString -> (Int, HashMapStrict.HashMap ProtocolBuffersBasic.ByteString Int, ProtocolBuffersBasic.Seq ProtocolBuffersBasic.Utf8)
newKeys (KeyStore keyCount keyMap keyList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq ProtocolBuffersBasic.Utf8) (keyCount, keyMap, keyList)

newValues :: Foldable t => ValueStore -> t VectorTile.Val -> (Int, HashMapStrict.HashMap VectorTile.Val Int, ProtocolBuffersBasic.Seq VectorTileInternal.Value)
newValues (ValueStore valueCount valueMap valueList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq VectorTileInternal.toProtobuf) (valueCount, valueMap, valueList)

addKeyValue :: (Eq a, Hashable.Hashable a) => Int -> a -> HashMapStrict.HashMap a Int -> Sequence.Seq b -> (a -> b)-> (Int, HashMapStrict.HashMap a Int, Sequence.Seq b)
addKeyValue currentKeyNumber key hashMap seqs f =
  case HashMapStrict.lookup key hashMap of
    Nothing -> (currentKeyNumber + 1, HashMapStrict.insert key currentKeyNumber hashMap, seqs Sequence.|> f key)
    Just _  -> (currentKeyNumber, hashMap, seqs)

newConvertGeometry :: Sequence.Seq Feature.Feature -> Word -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val -> HashMapStrict.HashMap ByteStringLazy.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> Geospatial.GeospatialGeometry -> ProtocolBuffersBasic.Seq Feature.Feature
newConvertGeometry acc fid convertedProps keys values geom =
  case geom of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> checkAndAdd keys values GeomType.POINT (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertPoint g)) acc
    Geospatial.MultiPoint g   -> checkAndAdd keys values GeomType.POINT (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiPoint g)) acc
    Geospatial.Line g         -> checkAndAdd keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertLineString g)) acc
    Geospatial.MultiLine g    -> checkAndAdd keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiLineString g)) acc
    Geospatial.Polygon g      -> checkAndAdd keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertPolygon g)) acc
    Geospatial.MultiPolygon g -> checkAndAdd keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (TypesGeoJsonFeatures.convertMultiPolygon g)) acc
    Geospatial.Collection gs -> Foldable.foldMap (newConvertGeometry acc fid convertedProps keys values) gs

checkAndAdd :: (VectorTileInternal.ProtobufGeom g, VectorTileInternal.GeomVec g ~ ProtocolBuffersBasic.Seq a) => HashMapStrict.HashMap ProtocolBuffersBasic.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> GeomType.GeomType -> VectorTile.Feature (ProtocolBuffersBasic.Seq a) -> ProtocolBuffersBasic.Seq Feature.Feature -> ProtocolBuffersBasic.Seq Feature.Feature
checkAndAdd keys values featureType feature@(VectorTile.Feature _ _ geoms) acc =
  if Sequence.null geoms
    then acc
    else VectorTileInternal.unfeats keys values featureType feature Sequence.<| acc
