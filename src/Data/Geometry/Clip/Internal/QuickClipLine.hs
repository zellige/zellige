{-# LANGUAGE FlexibleContexts #-}

-- QuickClip line clipping
-- https://ieeexplore.ieee.org/document/694214/

module Data.Geometry.Clip.Internal.QuickClipLine (
  clipLinesLb
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLinesLb :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLinesLb bb = Vector.foldl' (\acc lineString -> maybeAddLine acc (lineToClippedPoints bb lineString)) Vector.empty

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector VectorTile.Point -> Vector.Vector VectorTile.LineString
maybeAddLine acc pts =
    case ClipLine.checkValidLineString pts of
      Nothing  -> acc
      Just res -> Vector.cons res acc

lineToClippedPoints :: TypesGeography.BoundingBoxPts -> VectorTile.LineString -> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb lineString = foldPointsToLine $ VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.getLines lineString)

foldPointsToLine :: VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.StorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty

clipOrDiscard :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> VectorStorable.cons clippedLine acc

foldLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
foldLine bbox line = do
  checkAllCoordinates <- checkX (False, bbox, line) >>= checkY >>= checkX1 >>= checkY1 >>= checkX2 >>= checkY2
  reflectResult checkAllCoordinates
  where
    reflectResult (reflect, _, newLine@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2))) =
      if reflect then
        Just $ TypesGeography.StorableLine (VectorTile.Point x1 ((-1) * y1)) (VectorTile.Point x2 ((-1) * y2))
      else
        Just newLine

checkX :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkX (_, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point minX _) (VectorTile.Point maxX _)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | x1 > x2 =
    if x2 > maxX || x1 < minX then
      Nothing
    else
      Just (False, bbox, TypesGeography.StorableLine (VectorTile.Point x2 y2) (VectorTile.Point x1 y1))
  | x1 > maxX || x2 < minX = Nothing
  | otherwise = Just (False, bbox, line)

checkY :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkY (reflect, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point maxX maxY)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | y1 > y2 =
    if y2 > minY || y1 < maxY then
      Nothing
    else
      Just (True, newBbox, newLine)
  | y1 > minY || y2 < maxY = Nothing
  | otherwise = Just (reflect, bbox, line)
  where
    newBbox = TypesGeography.BoundingBoxPts (VectorTile.Point minX ((-1) * minY)) (VectorTile.Point maxX ((-1) * maxY))
    newLine = TypesGeography.StorableLine (VectorTile.Point x1 ((-1) * y1)) (VectorTile.Point x2 ((-1) * y2))

checkX1 :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkX1 (reflect, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point _ _)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | x1 < minX =
    if newY1 > minY then
      Nothing
    else
      Just (reflect, bbox, TypesGeography.StorableLine (VectorTile.Point minX newY1) (VectorTile.Point x2 y2))
  | otherwise = Just (reflect, bbox, line)
  where
    -- y1 += (xL - x1) * (y2 - y1) / (x2 - x1)
    newY1 = y1 + ((minX - x1) * (y2 - y1) `div` (x2 - x1))

checkY1 :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkY1 (reflect, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point _ _) (VectorTile.Point maxX maxY)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | y1 < maxY =
    if newX1 > maxX then
      Nothing
    else
      Just (reflect, bbox, TypesGeography.StorableLine (VectorTile.Point newX1 maxY) (VectorTile.Point x2 y2))
  | otherwise = Just (reflect, bbox, line)
  where
    -- x1 += (yB - y1) * (x2 - x1) / (y2 - y1)
    newX1 = x1 + ((maxY - y1) * (x2 - x1) `div` (y2 - y1))

checkX2 :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkX2 (reflect, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point _ _) (VectorTile.Point maxX _)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | x2 > maxX = Just (reflect, bbox, TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point maxX newY2))
  | otherwise = Just (reflect, bbox, line)
  where
    -- y2 = y1 + (xR - x1) * (y2 - y1) / (x2 - x1);
    newY2 = y1 + (maxX - x1) * (y2 - y1) `div` (x2 - x1)

checkY2 :: (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine) -> Maybe (Bool, TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkY2 (reflect, bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point _ _)), line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)))
  | y2 > minY = Just (reflect, bbox, TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point newX2 minX))
  | otherwise = Just (reflect, bbox, line)
  where
    --  x2 = x1 + (yT - y1) * (x2 - x1) / (y2 - y1);
    newX2 = x1 + (minY - y1) * (x2 - x1) `div` (y2 - y1)
