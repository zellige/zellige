module Data.Geometry.Clip.Internal.Point (
 clipPoints
) where

import qualified Data.Vector.Storable      as DataVectorStorable
import qualified Geography.VectorTile      as VectorTile

import           Data.Geometry.Types.Types

clipPoints :: BoundingBoxPts -> DataVectorStorable.Vector VectorTile.Point -> DataVectorStorable.Vector VectorTile.Point
clipPoints = DataVectorStorable.filter . pointInsideExtent

pointInsideExtent :: BoundingBoxPts -> VectorTile.Point -> Bool
pointInsideExtent BoundingBoxPts{_bbMinPts = (VectorTile.Point minX minY), _bbMaxPts = (VectorTile.Point maxX maxY)} (VectorTile.Point x y) = x >= minX && x <= maxX && y >= minY && y <= maxY

