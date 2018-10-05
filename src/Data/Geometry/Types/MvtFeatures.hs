{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Control.Foldl                                                    as Foldl
import qualified Data.Aeson                                                       as Aeson
import qualified Data.Aeson.Types                                                 as AesonTypes
import qualified Data.ByteString.Lazy                                             as ByteStringLazy
import qualified Data.Foldable                                                    as Foldable
import qualified Data.Geospatial                                                  as Geospatial
import qualified Data.Hashable                                                    as Hashable
import qualified Data.HashMap.Strict                                              as HashMapStrict
import qualified Data.LinearRing                                                  as LinearRing
import qualified Data.LineString                                                  as LineString
import qualified Data.List                                                        as List
import           Data.Monoid
import qualified Data.Scientific                                                  as Scientific
import qualified Data.Semigroup                                                   as Semigroup
import qualified Data.Sequence                                                    as Sequence
import qualified Data.Text                                                        as Text
import qualified Data.Text.Encoding                                               as TextEncoding
import qualified Data.Vector                                                      as Vector
import qualified Data.Vector.Storable                                             as VectorStorable
import qualified Geography.VectorTile                                             as VectorTile
import qualified Geography.VectorTile.Internal                                    as VectorTileInternal
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile          as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature  as Feature
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer    as Layer
import           Prelude                                                          hiding
                                                                                   (Left,
                                                                                   Right)
import qualified Text.ProtocolBuffers.Basic                                       as ProtocolBuffersBasic
import qualified Text.ProtocolBuffers.WireMessage                                 as WireMessage

data MvtFeatures = MvtFeatures
  { mvtPoints   :: Vector.Vector (VectorTile.Feature (VectorStorable.Vector VectorTile.Point))
  , mvtLines    :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.LineString))
  , mvtPolygons :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Polygon))
  } deriving (Eq, Show)

emptyMvtFeatures :: MvtFeatures
emptyMvtFeatures = MvtFeatures mempty mempty mempty

mkPoint :: Word -> Aeson.Value -> VectorStorable.Vector VectorTile.Point -> Vector.Vector (VectorTile.Feature (VectorStorable.Vector VectorTile.Point)) -> Vector.Vector (VectorTile.Feature (VectorStorable.Vector VectorTile.Point))
mkPoint fId props p = Vector.cons (VectorTile.Feature fId (convertProps props) p)

mkLineString :: Word -> Aeson.Value -> Vector.Vector VectorTile.LineString -> Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.LineString)) -> Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.LineString))
mkLineString fId props l = Vector.cons (mkFeature fId props l)

mkPolygon :: Word -> Aeson.Value -> Vector.Vector VectorTile.Polygon -> Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Polygon)) -> Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Polygon))
mkPolygon x props o = Vector.cons (mkFeature x props o)

mkFeature :: Word -> Aeson.Value -> Vector.Vector g -> VectorTile.Feature (Vector.Vector g)
mkFeature fId props = VectorTile.Feature fId (convertProps props)

convertProps :: Aeson.Value -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val
convertProps (Aeson.Object !x) = HashMapStrict.foldrWithKey (\k v acc -> maybe acc (\(!k', !v') -> HashMapStrict.insert k' v' acc) (convertElems (k, v))) HashMapStrict.empty x
convertProps _                 = HashMapStrict.empty

convertElems :: (Text.Text, Aeson.Value) -> Maybe (ByteStringLazy.ByteString, VectorTile.Val)
convertElems (!k, Aeson.String !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.St ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) v))
convertElems (!k, Aeson.Number !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.Do (sToF v))
convertElems (!k, Aeson.Bool !v)   = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.B v)
convertElems _                   = Nothing

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

foldLayer :: Foldl.Fold (Geospatial.GeospatialGeometry, AesonTypes.Value) StreamingLayer
foldLayer = Foldl.Fold step begin done
  where
    begin = StreamingLayer 1 (KeyStore 0 mempty mempty) (ValueStore 0 mempty mempty) mempty

    step (StreamingLayer featureId ks vs features) (geom, value) = StreamingLayer (featureId + 1) (KeyStore newKeyCount newKeyStore newKeyList) (ValueStore newValueCount newValueStore newValueList) newFeatures
      where
        convertedProps = convertProps value
        (newKeyCount, newKeyStore, newKeyList) = newKeys ks (HashMapStrict.keys convertedProps)
        (newValueCount, newValueStore, newValueList) = newValues vs (HashMapStrict.elems convertedProps)
        newFeatures = newConvertGeometry features featureId convertedProps newKeyStore newValueStore geom

    done = id

newKeys :: Foldable t => KeyStore -> t ProtocolBuffersBasic.ByteString -> (Int, HashMapStrict.HashMap ProtocolBuffersBasic.ByteString Int, ProtocolBuffersBasic.Seq ProtocolBuffersBasic.Utf8)
newKeys (KeyStore keyCount keyMap keyList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq ProtocolBuffersBasic.Utf8) (keyCount, keyMap, keyList)

newValues :: Foldable t => ValueStore -> t VectorTile.Val -> (Int, HashMapStrict.HashMap VectorTile.Val Int, ProtocolBuffersBasic.Seq VectorTileInternal.Value)
newValues (ValueStore valueCount valueMap valueList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq VectorTileInternal.toProtobuf) (valueCount, valueMap, valueList)

addKeyValue :: (Eq a, Hashable.Hashable a) => Int -> a -> HashMapStrict.HashMap a Int -> Sequence.Seq b -> (a -> b)-> (Int, HashMapStrict.HashMap a Int, Sequence.Seq b)
addKeyValue currentKeyNumber key hashMap seqs f =
  case HashMapStrict.lookup key hashMap of
    Nothing -> (currentKeyNumber + 1, HashMapStrict.insert key currentKeyNumber hashMap, f key Sequence.<| seqs)
    Just _  -> (currentKeyNumber, hashMap, seqs)

newConvertGeometry :: Sequence.Seq Feature.Feature -> Word -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val -> HashMapStrict.HashMap ByteStringLazy.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> Geospatial.GeospatialGeometry -> ProtocolBuffersBasic.Seq Feature.Feature
newConvertGeometry acc fid convertedProps keys values geom =
  case geom of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> VectorTileInternal.unfeats keys values GeomType.POINT (VectorTile.Feature fid convertedProps (convertPoint g)) Sequence.<| acc
    Geospatial.MultiPoint g   -> VectorTileInternal.unfeats keys values GeomType.POINT (VectorTile.Feature fid convertedProps (convertMultiPoint g)) Sequence.<| acc
    Geospatial.Line g         -> VectorTileInternal.unfeats keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (convertLineString g)) Sequence.<| acc
    Geospatial.MultiLine g    -> VectorTileInternal.unfeats keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (convertMultiLineString g)) Sequence.<| acc
    Geospatial.Polygon g      -> VectorTileInternal.unfeats keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (convertPolygon g)) Sequence.<| acc
    Geospatial.MultiPolygon g -> VectorTileInternal.unfeats keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (convertMultiPolygon g)) Sequence.<| acc
    Geospatial.Collection gs  -> Foldable.foldMap (newConvertGeometry acc fid convertedProps keys values) gs

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
  VectorStorable.uniq .
  Vector.convert .
  VectorStorable.foldr (mappend . coordsToPoints) mempty .
  LineString.toVector .
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
mkPolyPoints = VectorStorable.uniq . Vector.convert . LinearRing.foldMap coordsToPoints

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Vector.Vector VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector VectorTile.Point
coordsToPoints geoPosition = VectorStorable.singleton newPoint
    where
      newPoint = VectorTile.Point (round posX) (round posY)
      (Geospatial.PointXY posX posY) = Geospatial.retrieveXY geoPosition

instance Semigroup.Semigroup MvtFeatures where
  (<>) a b = MvtFeatures (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif
