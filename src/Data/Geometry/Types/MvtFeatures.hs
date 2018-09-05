{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.HashMap.Strict  as HashMapStrict
import qualified Data.Maybe           as Maybe
import           Data.Monoid
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TextEncoding
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Geography.VectorTile as VectorTile
import           Prelude              hiding (Left, Right)

data MvtFeatures = MvtFeatures
  { mvtPoints   :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Point))
  , mvtLines    :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.LineString))
  , mvtPolygons :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Polygon))
  } deriving (Eq, Show)

mkPoint :: Word -> Aeson.Value -> Vector.Vector VectorTile.Point -> MvtFeatures
mkPoint fId props p = MvtFeatures (mkFeaturePts fId props p) (Vector.empty :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.LineString))) (Vector.empty :: Vector.Vector (VectorTile.Feature (Vector.Vector VectorTile.Polygon)))

mkLineString :: Word -> Aeson.Value -> Vector.Vector VectorTile.LineString -> MvtFeatures
mkLineString fId props l = MvtFeatures mempty (mkFeature fId props l) mempty

mkPolygon :: Word -> Aeson.Value -> Vector.Vector VectorTile.Polygon -> MvtFeatures
mkPolygon x props o = MvtFeatures mempty mempty (mkFeature x props o)

mkFeaturePts :: Word -> Aeson.Value -> Vector.Vector g -> Vector.Vector (VectorTile.Feature (Vector.Vector g))
mkFeaturePts fId props geoms = Vector.singleton $ VectorTile.Feature fId (convertProps props) geoms

mkFeature :: Word -> Aeson.Value -> Vector.Vector g -> Vector.Vector (VectorTile.Feature (Vector.Vector g))
mkFeature fId props geoms = Vector.singleton $ VectorTile.Feature fId (convertProps props) geoms

convertProps :: Aeson.Value -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val
convertProps (Aeson.Object x) = HashMapStrict.fromList . Maybe.catMaybes $ Prelude.fmap convertElems (HashMapStrict.toList x)
convertProps _ = HashMapStrict.empty

convertElems :: (Text.Text, Aeson.Value) -> Maybe (ByteStringLazy.ByteString, VectorTile.Val)
convertElems (k, Aeson.String v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.St ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) v))
convertElems (k, Aeson.Number v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.Do (sToF v))
convertElems (k, Aeson.Bool v)   = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.B v)
convertElems _               = Nothing

sToF :: Scientific.Scientific -> Double
sToF = Scientific.toRealFloat

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty
  mappend a b = MvtFeatures (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)
