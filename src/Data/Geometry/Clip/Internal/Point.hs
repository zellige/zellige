module Data.Geometry.Clip.Internal.Point (
 clipPoints
) where

import qualified Data.Vector.Storable          as DataVectorStorable
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

clipPoints :: TypesGeography.BoundingBoxPts -> DataVectorStorable.Vector VectorTile.Point -> DataVectorStorable.Vector VectorTile.Point
clipPoints = DataVectorStorable.filter . pointInsideExtent

pointInsideExtent :: TypesGeography.BoundingBoxPts -> VectorTile.Point -> Bool
pointInsideExtent TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VectorTile.Point minX minY), TypesGeography._bbMaxPts = (VectorTile.Point maxX maxY)} (VectorTile.Point x y) = x >= minX && x <= maxX && y >= minY && y <= maxY

