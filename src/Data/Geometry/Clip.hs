{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBox
, clipPoint
, clipPoints
, clipLinesLb
, newClipLinesCs
, clipLinesQc
, clipLinesNLN
, clipPolygon
, clipPolygons
, clipPolygonQc
, clipPolygonsQc
) where

import           Data.Geometry.Clip.Internal.LineCohenSutherland
import           Data.Geometry.Clip.Internal.LineLiangBarsky
import           Data.Geometry.Clip.Internal.LineNichollLeeNicholl
import           Data.Geometry.Clip.Internal.LineQuickClip
import           Data.Geometry.Clip.Internal.Point
import           Data.Geometry.Clip.Internal.Polygon
import           Data.Geometry.Clip.Internal.PolygonQuickClip
import           Data.Geometry.Types.Geography

createBoundingBox :: Word -> Int -> BoundingBox
createBoundingBox buffer extent = BoundingBox (-fiBuffer) (-fiBuffer) (fiExtent + fiBuffer) (fiExtent + fiBuffer)
  where
    fiBuffer = fromIntegral buffer
    fiExtent = fromIntegral extent
