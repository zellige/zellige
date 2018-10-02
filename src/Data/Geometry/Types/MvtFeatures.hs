{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Control.Foldl                                                   as Foldl
import qualified Data.Aeson                                                      as Aeson
import qualified Data.Aeson.Types                                                as AesonTypes
import qualified Data.ByteString.Lazy                                            as ByteStringLazy
import qualified Data.Geospatial                                                 as Geospatial
import qualified Data.Hashable                                                   as Hashable
import qualified Data.HashMap.Strict                                             as HashMapStrict
import           Data.Monoid
import qualified Data.Scientific                                                 as Scientific
import qualified Data.Semigroup                                                  as Semigroup
import qualified Data.Sequence                                                   as Sequence
import qualified Data.Text                                                       as Text
import qualified Data.Text.Encoding                                              as TextEncoding
import qualified Geography.VectorTile                                            as VectorTile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature as Feature
import           Prelude                                                         hiding
                                                                                  (Left,
                                                                                  Right)
import qualified Text.ProtocolBuffers.Basic                                      as ProtocolBuffersBasic

data MvtFeatures = MvtFeatures
  { mvtPoints   :: !(Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Point)))
  , mvtLines    :: !(Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.LineString)))
  , mvtPolygons :: !(Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Polygon)))
  } deriving (Eq, Show)

emptyMvtFeatures :: MvtFeatures
emptyMvtFeatures = MvtFeatures mempty mempty mempty

mkPoint :: Word -> Aeson.Value -> Sequence.Seq VectorTile.Point -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Point)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Point))
mkPoint fId props p = (Sequence.<|) (VectorTile.Feature fId (convertProps props) p)

mkLineString :: Word -> Aeson.Value -> Sequence.Seq VectorTile.LineString -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.LineString)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.LineString))
mkLineString fId props l = (Sequence.<|) (mkFeature fId props l)

mkPolygon :: Word -> Aeson.Value -> Sequence.Seq VectorTile.Polygon -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Polygon)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Polygon))
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

-- Fold (x -> a -> x) x (x -> b) -- Fold step initial extract
data StreamingLayer = StreamingLayer
  { slKeyStore   :: KeyStore
  , slValueStore :: ValueStore
  , slFeatures   :: ProtocolBuffersBasic.Seq Feature.Feature
  }

data KeyStore = KeyStore
  { ksKeyInt :: Int
  , ksKeys   :: HashMapStrict.HashMap ByteStringLazy.ByteString Int
  }

data ValueStore = ValueStore
  { vsValueInt :: Int
  , vsValues   :: HashMapStrict.HashMap VectorTile.Val Int
  }

foldLayer :: Foldl.Fold (Geospatial.GeospatialGeometry, AesonTypes.Value) StreamingLayer
foldLayer = Foldl.Fold step begin done
  where
    begin = StreamingLayer (KeyStore 0 mempty) (ValueStore 0 mempty) mempty

    step (StreamingLayer (KeyStore keyCount keyMap) (ValueStore valueCount valueMap) features) (_, value) = StreamingLayer (KeyStore newKeyCount newKeyStore) (ValueStore newValueCount newValueStore) features
      where
        convertedProps = convertProps value
        (newKeyCount, newKeyStore) = foldr (\x (counter, currMap) -> addKeyValue counter x currMap) (keyCount, keyMap) (HashMapStrict.keys convertedProps)
        (newValueCount, newValueStore) = foldr (\x (counter, currMap) -> addKeyValue counter x currMap) (valueCount, valueMap) (HashMapStrict.elems convertedProps)

    done = id

addKeyValue :: (Eq a, Hashable.Hashable a) => Int -> a -> HashMapStrict.HashMap a Int -> (Int, HashMapStrict.HashMap a Int)
addKeyValue currentKeyNumber key hashMap =
  case HashMapStrict.lookup key hashMap of
    Nothing -> (currentKeyNumber + 1, HashMapStrict.insert key (currentKeyNumber + 1) hashMap)
    Just _  -> (currentKeyNumber, hashMap)

-- data MvtFeatures = MvtFeatures
--   { mvtPoints   :: !(Sequence.Seq (VectorTile.Feature VectorTileGeometry.Point))
--   , mvtLines    :: !(Sequence.Seq (VectorTile.Feature VectorTileGeometry.LineString))
--   , mvtPolygons :: !(Sequence.Seq (VectorTile.Feature VectorTileGeometry.Polygon))
--   } deriving (Eq, Show)

-- emptyMvtFeatures :: MvtFeatures
-- emptyMvtFeatures = MvtFeatures mempty mempty mempty

-- mkPoint :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.Point -> Sequence.Seq (VectorTile.Feature (Int, Int)) -> Sequence.Seq (VectorTile.Feature VectorTileGeometry.Point)
-- mkPoint fId props p = (Sequence.<|) (VectorTile.Feature (fromIntegral fId :: Int) (convertProps props) p)

-- mkLineString :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.LineString -> Sequence.Seq (VectorTile.Feature VectorTileGeometry.LineString) -> Sequence.Seq (VectorTile.Feature VectorTileGeometry.LineString)
-- mkLineString fId props l = (Sequence.<|) (mkFeature fId props l)

-- mkPolygon :: Word -> Aeson.Value -> Sequence.Seq VectorTileGeometry.Polygon -> Sequence.Seq (VectorTile.Feature VectorTileGeometry.Polygon) -> Sequence.Seq (VectorTile.Feature VectorTileGeometry.Polygon)
-- mkPolygon x props o = (Sequence.<|) (mkFeature x props o)

-- mkFeature :: Word -> Aeson.Value -> Sequence.Seq g -> VectorTile.Feature g
-- mkFeature fId props = VectorTile.Feature (fromIntegral fId :: Int) (convertProps props)

-- convertProps :: Aeson.Value -> MapLazy.Map Text.Text VectorTile.Val
-- convertProps (Aeson.Object !x) = HashMapStrict.foldrWithKey (\k v acc -> maybe acc (\(!k', !v') -> MapLazy.insert k' v' acc) (convertElems (k, v))) MapLazy.empty x
-- convertProps _                 = MapLazy.empty

-- convertElems :: (t, Aeson.Value) -> Maybe (t, VectorTile.Val)
-- convertElems (!k, Aeson.String !v) = Just (k, VectorTile.St v)
-- convertElems (!k, Aeson.Number !v) = Just (k, VectorTile.Do (sToF v))
-- convertElems (!k, Aeson.Bool !v)   = Just (k, VectorTile.B v)
-- convertElems _                     = Nothing

sToF :: Scientific.Scientific -> Double
sToF = Scientific.toRealFloat

instance Semigroup.Semigroup MvtFeatures where
  (<>) a b = MvtFeatures (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif
