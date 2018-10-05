{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Foldl                                                    as Foldl
import qualified Control.Monad.ST                                                 as MonadST
import qualified Data.Aeson                                                       as Aeson
import qualified Data.Aeson.Types                                                 as AesonTypes
import qualified Data.ByteString                                                  as ByteString
import qualified Data.ByteString.Lazy                                             as ByteStringLazy
import qualified Data.Foldable                                                    as Foldable
import qualified Data.Geospatial                                                  as Geospatial
import qualified Data.Hashable                                                    as Hashable
import qualified Data.HashMap.Strict                                              as HashMapStrict
import qualified Data.LinearRing                                                  as LinearRing
import qualified Data.LineString                                                  as LineString
import qualified Data.List                                                        as List
-- import           Data.Monoid
-- import qualified Data.SeqHelper                                                   as SeqHelper
import qualified Data.Sequence                                                    as Sequence
import qualified Data.STRef                                                       as STRef
import qualified Data.Vector                                                      as Vector
import qualified Data.Vector.Storable                                             as VectorStorable
import qualified Geography.VectorTile                                             as VectorTile
import qualified Geography.VectorTile.Internal                                    as VectorTileInternal
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile          as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature  as Feature
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer    as Layer
import qualified Text.ProtocolBuffers.Basic                                       as ProtocolBuffersBasic
import qualified Text.ProtocolBuffers.WireMessage                                 as WireMessage

import qualified Data.Geometry.Types.Config                                       as TypesConfig
import qualified Data.Geometry.Types.MvtFeatures                                  as TypesMvtFeatures
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

    step (StreamingLayer featureId (KeyStore keyCount keyMap keyList) (ValueStore valueCount valueMap valueList) features) (geom, value) = StreamingLayer (featureId + 1) (KeyStore newKeyCount newKeyStore newKeyList) (ValueStore newValueCount newValueStore newValueList) newFeatures
      where
        convertedProps = TypesMvtFeatures.convertProps value
        (newKeyCount, newKeyStore, newKeyList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq ProtocolBuffersBasic.Utf8) (keyCount, keyMap, keyList) (HashMapStrict.keys convertedProps)
        (newValueCount, newValueStore, newValueList) = foldr (\x (counter, currMap, currSeq) -> addKeyValue counter x currMap currSeq VectorTileInternal.toProtobuf) (valueCount, valueMap, valueList) (HashMapStrict.elems convertedProps)
        newFeatures = newConvertGeometry features featureId convertedProps newKeyStore newValueStore geom

    done = id

addKeyValue :: (Eq a, Hashable.Hashable a) => Int -> a -> HashMapStrict.HashMap a Int -> Sequence.Seq b -> (a -> b)-> (Int, HashMapStrict.HashMap a Int, Sequence.Seq b)
addKeyValue currentKeyNumber key hashMap seqs f =
  case HashMapStrict.lookup key hashMap of
    Nothing -> (currentKeyNumber + 1, HashMapStrict.insert key currentKeyNumber hashMap, f key Sequence.<| seqs)
    Just _  -> (currentKeyNumber, hashMap, seqs)

newConvertGeometry :: Sequence.Seq Feature.Feature -> Word -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val -> HashMapStrict.HashMap ByteStringLazy.ByteString Int -> HashMapStrict.HashMap VectorTile.Val Int -> Geospatial.GeospatialGeometry -> ProtocolBuffersBasic.Seq Feature.Feature
newConvertGeometry acc fid convertedProps keys values geom =
  case geom of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.POINT (VectorTile.Feature fid convertedProps (convertPoint g)))
    Geospatial.MultiPoint g   -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.POINT (VectorTile.Feature fid convertedProps (convertMultiPoint g)))
    Geospatial.Line g         -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (convertLineString g)))
    Geospatial.MultiLine g    -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.LINESTRING (VectorTile.Feature fid convertedProps (convertMultiLineString g)))
    Geospatial.Polygon g      -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (convertPolygon g)))
    Geospatial.MultiPolygon g -> acc <> pure (VectorTileInternal.unfeats keys values GeomType.POLYGON (VectorTile.Feature fid convertedProps (convertMultiPolygon g)))
    Geospatial.Collection gs  -> Foldable.foldMap (newConvertGeometry acc fid convertedProps keys values) gs

createLayerFromStreamingLayer :: TypesConfig.Config -> StreamingLayer -> Layer.Layer
createLayerFromStreamingLayer TypesConfig.Config{..} (StreamingLayer _ (KeyStore _ _ keysList) (ValueStore _ _ valuesList) features) = Layer.Layer
  { Layer.version   = fromIntegral _version
  , Layer.name      = ProtocolBuffersBasic.Utf8 _name
  , Layer.features  = features
  , Layer.keys      = keysList
  , Layer.values    = valuesList
  , Layer.extent    = Just $ fromIntegral _extents
  , Layer.ext'field = ProtocolBuffersBasic.defaultValue
  }

createTileFromStreamingLayer :: TypesConfig.Config -> StreamingLayer -> Tile.Tile
createTileFromStreamingLayer config sl = Tile.Tile
  { Tile.layers    = Sequence.fromList [createLayerFromStreamingLayer config sl]
  , Tile.ext'field = ProtocolBuffersBasic.defaultValue
  }

vtToBytes :: TypesConfig.Config -> StreamingLayer -> ByteString.ByteString
vtToBytes config sl = ByteStringLazy.toStrict . WireMessage.messagePut $ createTileFromStreamingLayer config sl

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

