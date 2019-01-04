{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.GeoJsonFeatures where

import qualified Data.Foldable        as Foldable
import qualified Data.Geospatial      as Geospatial
import qualified Data.LinearRing      as LinearRing
import qualified Data.List            as List
import           Data.Monoid
import qualified Data.Scientific      as Scientific
import qualified Data.Semigroup       as Semigroup
import qualified Data.SeqHelper       as SeqHelper
import qualified Data.Sequence        as Sequence
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
convertLineString l =
  if Sequence.length convertedLine > 1
    then Sequence.singleton $ VectorTile.LineString convertedLine
    else Sequence.empty
  where
    convertedLine = convertAndRemoveDupes . Geospatial._unGeoLine $ l

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
        VectorTile.Polygon (convertAndRemoveDupes h) (mkPolys rest)
  where
    rawPoly = Geospatial._unGeoPolygon poly

-- Foldr?
mkPolys :: Foldable t => t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Sequence.Seq VectorTile.Polygon
mkPolys = List.foldl' (\acc lring -> (mkPoly lring Sequence.<| acc)) Sequence.empty

mkPoly :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly lring = VectorTile.Polygon (convertAndRemoveDupes lring) mempty

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Sequence.Seq VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers
-- Foldr?
convertAndRemoveDupes :: Foldable t => t Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
convertAndRemoveDupes = SeqHelper.removeNextDuplicate . Foldable.foldMap coordsToPoints

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
coordsToPoints geoPosition = Sequence.singleton newPoint
  where
    newPoint = VectorTile.Point (round posX) (round posY)
    (Geospatial.PointXY posX posY) = Geospatial.retrieveXY geoPosition
