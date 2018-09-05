{-# LANGUAGE FlexibleContexts #-}

-- QuickClip line clipping
-- https://ieeexplore.ieee.org/document/694214/

module Data.Geometry.Clip.Internal.LineQuickClip (
  clipLinesQc
  , clipOrDiscard
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

clipLinesQc :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLinesQc bb = Vector.foldl' (\acc lineString -> maybeAddLine acc (lineToClippedPoints (TypesGeography.bboxPtsToBbox bb) lineString)) Vector.empty

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector VectorTile.Point -> Vector.Vector VectorTile.LineString
maybeAddLine acc pts =
    case ClipLine.checkValidLineString pts of
      Nothing  -> acc
      Just res -> Vector.cons res acc

lineToClippedPoints :: TypesGeography.BoundingBox -> VectorTile.LineString -> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb lineString = ClipLine.foldPointsToLine $ VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.getLines lineString)

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> VectorStorable.cons clippedLine acc

data Line = Line
  { _x1 :: !Double
  , _y1 :: !Double
  , _x2 :: !Double
  , _y2 :: !Double
  } deriving (Show, Eq)

storableLineToLine :: TypesGeography.StorableLine -> Line
storableLineToLine (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = Line (fromIntegral x1 :: Double) (fromIntegral y1 :: Double) (fromIntegral x2 :: Double) (fromIntegral y2 :: Double)

foldLine :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
foldLine bbox line = do
  checkAllCoordinates <- checkX (False, False, bbox, storableLineToLine line) >>= checkY >>= checkX1 >>= checkY1 >>= checkX2 >>= checkY2 >>= switchBack
  reflectResult checkAllCoordinates
  where
    reflectResult (reflect, _, _, Line x1 y1 x2 y2) =
      if reflect then
        Just $ TypesGeography.StorableLine (VectorTile.Point (round x1) (round $ (-1) * y1)) (VectorTile.Point (round x2) (round $ (-1) * y2))
      else
        Just $ TypesGeography.StorableLine (VectorTile.Point (round x1) (round y1)) (VectorTile.Point (round x2) (round y2))

switchBack :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
switchBack (reflect, switched, bbox, line@(Line x1 y1 x2 y2)) =
  if switched then
    Just (reflect, switched, bbox, Line x2 y2 x1 y1)
  else
    Just (reflect, switched, bbox, line)

-- if (x0 > x1) {
--   if (x1 > xR || x0 < xL) return;
--   t = x0;
--   u = y0;
--   x0 = x1;
--   y0 = y1;
--   x1 = t;
--   y1 = u;
--} else if (x0 > xR || x1 < xL) return;
checkX :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkX (reflect, switched, bbox@(TypesGeography.BoundingBox minX _ maxX _), line@(Line x1 y1 x2 y2))
  | x1 > x2 =
    if x2 > maxX || x1 < minX then
      Nothing
    else
      Just (reflect, True, bbox, Line x2 y2 x1 y1)
  | x1 > maxX || x2 < minX = Nothing
  | otherwise = Just (reflect, switched, bbox, line)

-- if (y0 > y1) {
--   if (y1 > yT || y0 < yB) return;
--   reflect = TRUE;
--   y0 = -y0;
--   y1 = -y1;
--   temp = yB;
--   yB = -yT;
--   yT = -temp;
-- } else {
--   if (y0 > yT || y1 < yB) return;
--   reflect = FALSE;
-- }
checkY :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkY (_, switched, bbox@(TypesGeography.BoundingBox minX minY maxX maxY), line@(Line x1 y1 x2 y2))
  | y1 > y2 =
    if y2 > maxY || y1 < minY then
      Nothing
    else
      Just (True, switched, newBbox, newLine)
  | y1 > maxY || y2 < minY = Nothing
  | otherwise = Just (False, switched, bbox, line)
  where
    newBbox = TypesGeography.BoundingBox minX ((-1) * maxY) maxX ((-1) * minY)
    newLine = Line x1 ((-1) * y1) x2 ((-1) * y2)

-- if (x0 < xL) {
--   y0 += (xL - x0) * (y1 - y0) / (x1 - x0)
--   if (y0 > yT)
--     return;
--   x0 = xL;
-- }
checkX1 :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkX1 (reflect, switched, bbox@(TypesGeography.BoundingBox minX _ _ maxY), line@(Line x1 y1 x2 y2))
  | x1 < minX =
    if newY1 > maxY then
      Nothing
    else
      Just (reflect, switched, bbox, Line minX newY1 x2 y2)
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newY1 = y1 + ((minX - x1) * (y2 - y1) / (x2 - x1))

-- if (y0 < yB) {
--   x0 += (yB - y0) * (x1 - x0) / (y1 - y0)
--   if (x0 > xR)
--     return;
--   y0 = yB;
-- }
checkY1 :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkY1 (reflect, switched, bbox@(TypesGeography.BoundingBox _ minY maxX _), line@(Line x1 y1 x2 y2))
  | y1 < minY =
    if newX1 > maxX then
      Nothing
    else
      Just (reflect, switched, bbox, Line newX1 minY x2 y2)
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newX1 = x1 + ((minY - y1) * (x2 - x1) / (y2 - y1))

-- if (x1 > xR) {
--   y1 = y0 + (xR - x0) * (y1 - y0) / (x1 - x0);
--   x1 = xR;
-- }
checkX2 :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkX2 (reflect, switched, bbox@(TypesGeography.BoundingBox _ _ maxX _), line@(Line x1 y1 x2 y2))
  | x2 > maxX = Just (reflect, switched, bbox, Line x1 y1 maxX newY2)
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newY2 = y1 + (maxX - x1) * (y2 - y1) / (x2 - x1)

-- if (y1 > yT) {
--   x1 = x0 + (yT - y0) * (x1 - x0) / (y1 - y0);
--   y1 = yT;
-- }
checkY2 :: (Bool, Bool, TypesGeography.BoundingBox, Line) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, Line)
checkY2 (reflect, switched, bbox@(TypesGeography.BoundingBox _ _ _ maxY), line@(Line x1 y1 x2 y2))
  | y2 > maxY = Just (reflect, switched, bbox, Line x1 y1 newX2 maxY)
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newX2 = x1 + (maxY - y1) * (x2 - x1) / (y2 - y1)
