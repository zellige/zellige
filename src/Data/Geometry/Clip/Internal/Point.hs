{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.Point (
 clipPoints
) where

import qualified Data.Sequence             as DS
import qualified Geography.VectorTile      as VG

import           Data.Geometry.Types.Types

clipPoints :: BoundingBoxPts -> DS.Seq VG.Point -> DS.Seq VG.Point
clipPoints = DS.filter . pointInsideExtent

pointInsideExtent :: BoundingBoxPts -> VG.Point -> Bool
pointInsideExtent BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

