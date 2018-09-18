{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
module Data.Geometry.Clip.Internal.Line where

import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geospatial               as Geospatial
import qualified Data.LineString               as LineString
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

checkValidLineString :: VectorStorable.Vector VectorTile.Point -> Maybe VectorTile.LineString
checkValidLineString pts =
  if VectorStorable.length (segmentToLine pts) >= 2
    then Just (VectorTile.LineString (segmentToLine pts))
    else Nothing
{-# INLINE checkValidLineString #-}

getLines :: VectorTile.LineString -> VectorStorable.Vector TypesGeography.StorableLine
getLines line = linesFromPoints $ VectorTile.lsPoints line
{-# INLINE getLines #-}

newGetLines :: Geospatial.GeoLine -> Vector.Vector TypesGeography.GeoStorableLine
newGetLines (Geospatial.GeoLine line) = newLinesFromPoints line
{-# INLINE newGetLines #-}

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector TypesGeography.StorableLine
linesFromPoints x = (VectorStorable.zipWith TypesGeography.StorableLine <*> VectorStorable.tail) (VectorStorable.convert x)
{-# INLINE linesFromPoints #-}

newLinesFromPoints :: LineString.LineString Geospatial.GeoPositionWithoutCRS -> Vector.Vector TypesGeography.GeoStorableLine
newLinesFromPoints = LineString.combineToVector (\x y -> TypesGeography.GeoStorableLine (Geospatial.retrieveXY x) (Geospatial.retrieveXY y))
{-# INLINE newLinesFromPoints #-}

pointsFromLine :: TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.PointXY
pointsFromLine (TypesGeography.GeoStorableLine p1 p2) = Vector.fromList [p1, p2]
{-# INLINE pointsFromLine #-}

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
segmentToLine l = if VectorStorable.length l > 1 then VectorStorable.cons start (second l) else mempty
  where
    start = VectorStorable.head l
    second = VectorStorable.ifilter (\i _ -> odd i)
{-# INLINE segmentToLine #-}

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
newSegmentToLine :: Vector.Vector Geospatial.GeoPositionWithoutCRS -> Vector.Vector Geospatial.GeoPositionWithoutCRS
newSegmentToLine l = if Vector.length l > 1 then Vector.cons start (second l) else mempty
  where
    start = Vector.head l
    second = Vector.ifilter (\i _ -> odd i)
{-# INLINE newSegmentToLine #-}

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
newNewFoldPointsToLine :: Vector.Vector TypesGeography.GeoStorableLine -> Vector.Vector Geospatial.GeoPositionWithoutCRS
newNewFoldPointsToLine = newSegmentToLine . Vector.foldr (mappend . (\(TypesGeography.GeoStorableLine p1 p2) -> Vector.fromList [Geospatial.GeoPointXY p1, Geospatial.GeoPointXY p2])) mempty
{-# INLINE newNewFoldPointsToLine #-}

foldPointsToLine :: VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.StorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty
{-# INLINE foldPointsToLine #-}

newFoldPointsToLine :: Vector.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector VectorTile.Point
newFoldPointsToLine = Vector.foldr (mappend . (\(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) -> VectorStorable.fromList [VectorTile.Point (round x1) (round y1), VectorTile.Point (round x2) (round y2)])) mempty
{-# INLINE newFoldPointsToLine #-}
