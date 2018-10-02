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

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.HashMap.Strict  as HashMapStrict
import           Data.Monoid
import qualified Data.Scientific      as Scientific
import qualified Data.Semigroup       as Semigroup
import qualified Data.Sequence        as Sequence
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TextEncoding
import qualified Geography.VectorTile as VectorTile
import           Prelude              hiding (Left, Right)

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
