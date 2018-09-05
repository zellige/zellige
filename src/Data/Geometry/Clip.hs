{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBoxPts
, clipPoints
, clipLinesLb
, clipLinesCs
, clipLinesQc
, clipLinesNLN
, clipPolygon
, clipPolygons
, clipPolygonQc
, clipPolygonsQc
) where

import qualified Geography.VectorTile                            as VectorTile

import           Data.Geometry.Clip.Internal.LineCohenSutherland
import           Data.Geometry.Clip.Internal.LineLiangBarsky
import           Data.Geometry.Clip.Internal.LineQuickClip
import           Data.Geometry.Clip.Internal.LineNichollLeeNicholl
import           Data.Geometry.Clip.Internal.Point
import           Data.Geometry.Clip.Internal.Polygon
import           Data.Geometry.Clip.Internal.PolygonQuickClip
import           Data.Geometry.Types.Geography

createBoundingBoxPts :: Word -> Int -> BoundingBoxPts
createBoundingBoxPts buffer extent = BoundingBoxPts (VectorTile.Point (-iBuffer) (-iBuffer)) (VectorTile.Point (iExtent+iBuffer) (iExtent+iBuffer))
  where
    iBuffer = fromIntegral buffer
    iExtent = fromIntegral extent
