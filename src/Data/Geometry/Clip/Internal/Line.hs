{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
module Data.Geometry.Clip.Internal.Line where

import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

checkValidLineString :: VectorStorable.Vector VectorTile.Point -> Maybe VectorTile.LineString
checkValidLineString pts =
  if VectorStorable.length (segmentToLine pts) >= 2
    then Just (VectorTile.LineString (segmentToLine pts))
    else Nothing

getLines :: VectorTile.LineString -> VectorStorable.Vector TypesGeography.StorableLine
getLines line = linesFromPoints $ VectorTile.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector TypesGeography.StorableLine
linesFromPoints x = (VectorStorable.zipWith TypesGeography.StorableLine <*> VectorStorable.tail) (VectorStorable.convert x)

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
segmentToLine l = if VectorStorable.length l > 1 then VectorStorable.cons start (second l) else mempty
  where
    start = VectorStorable.head l
    second = VectorStorable.ifilter (\i _ -> odd i)
