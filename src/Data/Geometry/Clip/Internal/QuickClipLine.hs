{-# LANGUAGE FlexibleContexts #-}

-- QuickClip line clipping
-- https://ieeexplore.ieee.org/document/694214/

module Data.Geometry.Clip.Internal.QuickClipLine (
  clipLinesQc
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLinesQc :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLinesQc bb = Vector.foldl' (\acc lineString -> maybeAddLine acc (lineToClippedPoints (TypesGeography.bboxPtsToBbox bb) lineString)) Vector.empty

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector VectorTile.Point -> Vector.Vector VectorTile.LineString
maybeAddLine acc pts =
    case ClipLine.checkValidLineString pts of
      Nothing  -> acc
      Just res -> Vector.cons res acc

lineToClippedPoints :: TypesGeography.BoundingBox -> VectorTile.LineString -> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb lineString = foldPointsToLine $ VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.getLines lineString)

foldPointsToLine :: VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.StorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> VectorStorable.cons clippedLine acc

foldLine :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
foldLine bbox line = do
  checkAllCoordinates <- checkX (False, bbox, lineToDoublePts line) >>= checkY >>= checkX1 >>= checkY1 >>= checkX2 >>= checkY2
  reflectResult checkAllCoordinates
  where
    lineToDoublePts (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = ((fromIntegral x1 :: Double, fromIntegral y1 :: Double), (fromIntegral x2 :: Double, fromIntegral y2 :: Double))
    reflectResult (reflect, _, ((x1, y1), (x2, y2))) =
      if reflect then
        Just $ TypesGeography.StorableLine (VectorTile.Point (round x1) (round $ (-1) * y1)) (VectorTile.Point (round x2) (round $ (-1) * y2))
      else
        Just $ TypesGeography.StorableLine (VectorTile.Point (round x1) (round y1)) (VectorTile.Point (round x2) (round y2))

-- if (x0 > x1) {
--   if (x1 > xR || x0 < xL) return;
--   t = x0;
--   u = y0;
--   x0 = x1;
--   y0 = y1;
--   x1 = t;
--   y1 = u;
--} else if (x0 > xR || x1 < xL) return;
checkX :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkX (reflect, bbox@(TypesGeography.BoundingBox minX _ maxX _), line@((x1, y1), (x2, y2)))
  | x1 > x2 =
    if x2 > maxX || x1 < minX then
      Nothing
    else
      Just (reflect, bbox, ((x2, y2), (x1, y1)))
  | x1 > maxX || x2 < minX = Nothing
  | otherwise = Just (reflect, bbox, line)

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
checkY :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkY (_, bbox@(TypesGeography.BoundingBox minX minY maxX maxY), line@((x1, y1), (x2, y2)))
  | y1 > y2 =
    if y2 > minY || y1 < maxY then
      Nothing
    else
      Just (True, newBbox, newLine)
  | y1 > minY || y2 < maxY = Nothing
  | otherwise = Just (False, bbox, line)
  where
    newBbox = TypesGeography.BoundingBox minX ((-1) * maxY) maxX ((-1) * minY)
    newLine = ((x1, (-1) * y1), (x2, (-1) * y2))

-- if (x0 < xL) {
--   y0 += (xL - x0) * (y1 - y0) / (x1 - x0)
--   if (y0 > yT)
--     return;
--   x0 = xL;
-- }
checkX1 :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkX1 (reflect, bbox@(TypesGeography.BoundingBox minX minY _ _), line@((x1, y1), (x2, y2)))
  | x1 < minX =
    if newY1 > minY then
      Nothing
    else
      Just (reflect, bbox, ((minX, newY1), (x2, y2)))
  | otherwise = Just (reflect, bbox, line)
  where
    newY1 = y1 + ((minX - x1) * (y2 - y1) / (x2 - x1))

-- if (y0 < yB) {
--   x0 += (yB - y0) * (x1 - x0) / (y1 - y0)
--   if (x0 > xR)
--     return;
--   y0 = yB;
-- }
checkY1 :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkY1 (reflect, bbox@(TypesGeography.BoundingBox _ _ maxX maxY), line@((x1, y1), (x2, y2)))
  | y1 < maxY =
    if newX1 > maxX then
      Nothing
    else
      Just (reflect, bbox, ((newX1, maxY), (x2, y2)))
  | otherwise = Just (reflect, bbox, line)
  where
    newX1 = x1 + ((maxY - y1) * (x2 - x1) / (y2 - y1))

-- if (x1 > xR) {
--   y1 = y0 + (xR - x0) * (y1 - y0) / (x1 - x0);
--   x1 = xR;
-- }
checkX2 :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkX2 (reflect, bbox@(TypesGeography.BoundingBox _ _ maxX _), line@((x1, y1), (x2, y2)))
  | x2 > maxX = Just (reflect, bbox, ((x1, y1), (maxX, newY2)))
  | otherwise = Just (reflect, bbox, line)
  where
    newY2 = y1 + (maxX - x1) * (y2 - y1) / (x2 - x1)

-- if (y1 > yT) {
--   x1 = x0 + (yT - y0) * (x1 - x0) / (y1 - y0);
--   y1 = yT;
-- }
checkY2 :: (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double))) -> Maybe (Bool, TypesGeography.BoundingBox, ((Double, Double), (Double, Double)))
checkY2 (reflect, bbox@(TypesGeography.BoundingBox minX minY _ _), line@((x1, y1), (x2, y2)))
  | y2 > minY = Just (reflect, bbox, ((x1, y1), (newX2, minX)))
  | otherwise = Just (reflect, bbox, line)
  where
    newX2 = x1 + (minY - y1) * (x2 - x1) / (y2 - y1)
