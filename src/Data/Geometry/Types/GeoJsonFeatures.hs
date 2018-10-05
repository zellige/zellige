{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.GeoJsonFeatures where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Foldable        as Foldable
import qualified Data.Geospatial      as Geospatial
import qualified Data.HashMap.Strict  as HashMapStrict
import qualified Data.LinearRing      as LinearRing
import qualified Data.LineString      as LineString
import qualified Data.List            as List
import           Data.Monoid
import qualified Data.Scientific      as Scientific
import qualified Data.Semigroup       as Semigroup
import qualified Data.SeqHelper       as SeqHelper
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

instance Semigroup.Semigroup MvtFeatures where
  (<>) a b = MvtFeatures (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif

emptyMvtFeatures :: MvtFeatures
emptyMvtFeatures = MvtFeatures mempty mempty mempty

sToF :: Scientific.Scientific -> Double
sToF = Scientific.toRealFloat

-- Points

convertPoint :: Geospatial.GeoPoint -> Sequence.Seq VectorTile.Point
convertPoint = coordsToPoints . Geospatial._unGeoPoint

convertMultiPoint :: Geospatial.GeoMultiPoint -> Sequence.Seq VectorTile.Point
convertMultiPoint = Foldable.foldMap convertPoint . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: Geospatial.GeoLine -> Sequence.Seq VectorTile.LineString
convertLineString =
  Sequence.singleton .
  VectorTile.LineString .
  SeqHelper.unique .
  Foldable.foldMap coordsToPoints .
  LineString.fromLineString .
  Geospatial._unGeoLine

convertMultiLineString :: Geospatial.GeoMultiLine -> Sequence.Seq VectorTile.LineString
convertMultiLineString = Foldable.foldMap convertLineString . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: Geospatial.GeoPolygon -> Sequence.Seq VectorTile.Polygon
convertPolygon poly =
  Sequence.singleton $
  case Sequence.viewl rawPoly of
    Sequence.EmptyL -> VectorTile.Polygon mempty mempty
    (h Sequence.:< rest) ->
      if Sequence.length rest == 0 then
        mkPoly h
      else
        VectorTile.Polygon (mkPolyPoints h) (mkPolys rest)
  where
    rawPoly = Geospatial._unGeoPolygon poly

mkPolys :: Foldable t => t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Sequence.Seq VectorTile.Polygon
mkPolys = List.foldl' (\acc lring -> (mkPoly lring Sequence.<| acc)) Sequence.empty

mkPoly :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly lring = VectorTile.Polygon (mkPolyPoints lring) mempty

mkPolyPoints :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
mkPolyPoints = SeqHelper.unique . foldMap coordsToPoints

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Sequence.Seq VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
coordsToPoints geoPosition = Sequence.singleton newPoint
  where
    newPoint = VectorTile.Point (round posX) (round posY)
    (Geospatial.PointXY posX posY) = Geospatial.retrieveXY geoPosition
