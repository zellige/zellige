{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBoxPts
, clipPoints
, clipLines
, clipPolygon
, clipPolygons
) where

import           Data.Geometry.Clip.Internal.Line
import           Data.Geometry.Clip.Internal.Point
import           Data.Geometry.Clip.Internal.Polygon
import           Data.Geometry.Types.Types

createBoundingBoxPts :: Word -> Word -> BoundingBoxPts
createBoundingBoxPts buffer extent = BoundingBoxPts (-iBuffer, -iBuffer) (iExtent+iBuffer, iExtent+iBuffer)
  where
    iBuffer = fromIntegral buffer
    iExtent = fromIntegral extent

