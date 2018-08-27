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
    Nothing -> acc
    Just clippedLine -> VectorStorable.cons clippedLine acc

foldLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
foldLine bbox@(TypesGeography.BoundingBoxPts (VectorTile.Point minX _) (VectorTile.Point maxX _)) line@(TypesGeography.StorableLine (VectorTile.Point x1 _) (VectorTile.Point x2 _)) = 
  if (x1 > maxX) || (x2 < minX) then
    Nothing
  else
    case checkX bbox line of
      Nothing -> Nothing
      Just newLine -> 
        case checkY bbox newLine of
          Nothing -> Nothing
          Just newBboxAndLine -> Just $ snd newBboxAndLine

checkX :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
checkX (TypesGeography.BoundingBoxPts (VectorTile.Point minX _) (VectorTile.Point maxX _)) (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = 
  if x1 > x2 && (x2 > maxX || x1 < minX) then
      Nothing
    else
      Just $ TypesGeography.StorableLine (VectorTile.Point x2 y2) (VectorTile.Point x1 y1)

checkY :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe (TypesGeography.BoundingBoxPts, TypesGeography.StorableLine)
checkY (TypesGeography.BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point maxX maxY)) (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) =
  if y1 > y2 && (y2 > minY || y1 < maxY) then
    Nothing
  else
    Just (newBbox, newLine)
  where
    newBbox = TypesGeography.BoundingBoxPts (VectorTile.Point minX ((-1) * minY)) (VectorTile.Point maxX ((-1) * maxY))
    newLine = TypesGeography.StorableLine (VectorTile.Point x1 ((-1) * y1)) (VectorTile.Point x2 ((-1) * y2))
      