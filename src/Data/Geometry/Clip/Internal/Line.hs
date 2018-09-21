{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
module Data.Geometry.Clip.Internal.Line where

import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geospatial               as Geospatial
import qualified Data.LineString               as LineString
import qualified Data.Vector.Storable          as VectorStorable

getLines :: Geospatial.GeoLine -> VectorStorable.Vector TypesGeography.GeoStorableLine
getLines (Geospatial.GeoLine line) = linesFromPoints line
{-# INLINE getLines #-}

linesFromPoints :: LineString.LineString Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector TypesGeography.GeoStorableLine
linesFromPoints = LineString.combineToVector (\x y -> TypesGeography.GeoStorableLine (Geospatial.retrieveXY x) (Geospatial.retrieveXY y))
{-# INLINE linesFromPoints #-}

pointsFromLine :: TypesGeography.GeoStorableLine -> VectorStorable.Vector Geospatial.PointXY
pointsFromLine (TypesGeography.GeoStorableLine p1 p2) = VectorStorable.fromList [p1, p2]
{-# INLINE pointsFromLine #-}

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: VectorStorable.Vector Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector Geospatial.GeoPositionWithoutCRS
segmentToLine l = if VectorStorable.length l > 1 then VectorStorable.cons start (second l) else mempty
  where
    start = VectorStorable.head l
    second = VectorStorable.ifilter (\i _ -> odd i)
{-# INLINE segmentToLine #-}

-- Fold points from line to a vector of points
lineToGeoPoint :: VectorStorable.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector Geospatial.GeoPositionWithoutCRS
lineToGeoPoint = segmentToLine . VectorStorable.foldr (mappend . (\(TypesGeography.GeoStorableLine p1 p2) -> VectorStorable.fromList [Geospatial.GeoPointXY p1, Geospatial.GeoPointXY p2])) mempty
{-# INLINE lineToGeoPoint #-}

lineToPointXY :: VectorStorable.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector Geospatial.PointXY
lineToPointXY = VectorStorable.foldr (mappend . (\(TypesGeography.GeoStorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty
{-# INLINE lineToPointXY #-}
