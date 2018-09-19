{-# LANGUAGE FlexibleContexts #-}

-- QuickClip line clipping
-- https://ieeexplore.ieee.org/document/694214/

module Data.Geometry.Clip.Internal.LineQuickClip (
  clipLineQc
  , clipLinesQc
  , clipOrDiscard
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LineString                  as LineString
import qualified Data.Validation                  as Validation
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import           Prelude                          hiding (lines)

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

clipLineQc :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipLineQc bb line feature acc =
  case LineString.fromVector clippedLine of
    Validation.Success res -> Vector.cons (Geospatial.reWrapGeometry feature (Geospatial.Line (Geospatial.GeoLine res))) acc
    Validation.Failure _   -> acc
  where
    clippedLine = ClipLine.newNewFoldPointsToLine $ lineToClippedPoints bb line

clipLinesQc :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipLinesQc bb lines (Geospatial.GeoFeature bbox _ props fId) acc = checkLinesAndAdd
  where
    checkLinesAndAdd = if Vector.null multiLine then acc else Vector.cons reMakeFeature acc
    reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine multiLine)) props fId
    multiLine = Vector.foldl' maybeAddLine mempty (linesToClippedPoints bb (Geospatial.splitGeoMultiLine lines))

maybeAddLine :: Vector.Vector
               (LineString.LineString Geospatial.GeoPositionWithoutCRS)
             -> VectorStorable.Vector TypesGeography.GeoStorableLine
             -> Vector.Vector
                  (LineString.LineString Geospatial.GeoPositionWithoutCRS)
maybeAddLine acc pp =
  case clipLineToValidationLineString pp of
    Validation.Success res -> Vector.cons res acc
    Validation.Failure _   -> acc

clipLineToValidationLineString :: VectorStorable.Vector TypesGeography.GeoStorableLine -> Validation.Validation LineString.VectorToLineStringError (LineString.LineString Geospatial.GeoPositionWithoutCRS)
clipLineToValidationLineString lines = LineString.fromVector (ClipLine.newNewFoldPointsToLine lines)

lineToClippedPoints :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> VectorStorable.Vector TypesGeography.GeoStorableLine
lineToClippedPoints bb geoLine = VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.newGetLines geoLine)

linesToClippedPoints :: Functor f => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (VectorStorable.Vector TypesGeography.GeoStorableLine)
linesToClippedPoints bb = fmap (lineToClippedPoints bb)

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> VectorStorable.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector TypesGeography.GeoStorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> VectorStorable.cons clippedLine acc

foldLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> Maybe TypesGeography.GeoStorableLine
foldLine bbox line = do
  checkAllCoordinates <- checkX (False, False, bbox, line) >>= checkY >>= checkX1 >>= checkY1 >>= checkX2 >>= checkY2 >>= switchBack
  reflectResult checkAllCoordinates
  where
    reflectResult (reflect, _, _, newLine@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2))) =
      if reflect then
        Just $ TypesGeography.GeoStorableLine (Geospatial.PointXY x1 ((-1) * y1)) (Geospatial.PointXY x2 ((-1) * y2))
      else
        Just newLine

switchBack :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
switchBack (reflect, switched, bbox, line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2))) =
  if switched then
    Just (reflect, switched, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY x2 y2) (Geospatial.PointXY x1 y1))
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
checkX :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkX (reflect, switched, bbox@(TypesGeography.BoundingBox minX _ maxX _), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | x1 > x2 =
    if x2 > maxX || x1 < minX then
      Nothing
    else
      Just (reflect, True, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY x2 y2) (Geospatial.PointXY x1 y1))
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
checkY :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkY (_, switched, bbox@(TypesGeography.BoundingBox minX minY maxX maxY), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | y1 > y2 =
    if y2 > maxY || y1 < minY then
      Nothing
    else
      Just (True, switched, newBbox, newLine)
  | y1 > maxY || y2 < minY = Nothing
  | otherwise = Just (False, switched, bbox, line)
  where
    newBbox = TypesGeography.BoundingBox minX ((-1) * maxY) maxX ((-1) * minY)
    newLine = TypesGeography.GeoStorableLine (Geospatial.PointXY x1 ((-1) * y1)) (Geospatial.PointXY x2 ((-1) * y2))

-- if (x0 < xL) {
--   y0 += (xL - x0) * (y1 - y0) / (x1 - x0)
--   if (y0 > yT)
--     return;
--   x0 = xL;
-- }
checkX1 :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkX1 (reflect, switched, bbox@(TypesGeography.BoundingBox minX _ _ maxY), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | x1 < minX =
    if newY1 > maxY then
      Nothing
    else
      Just (reflect, switched, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY minX newY1) (Geospatial.PointXY x2 y2))
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newY1 = y1 + ((minX - x1) * (y2 - y1) / (x2 - x1))

-- if (y0 < yB) {
--   x0 += (yB - y0) * (x1 - x0) / (y1 - y0)
--   if (x0 > xR)
--     return;
--   y0 = yB;
-- }
checkY1 :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkY1 (reflect, switched, bbox@(TypesGeography.BoundingBox _ minY maxX _), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | y1 < minY =
    if newX1 > maxX then
      Nothing
    else
      Just (reflect, switched, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY newX1 minY) (Geospatial.PointXY x2 y2))
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newX1 = x1 + ((minY - y1) * (x2 - x1) / (y2 - y1))

-- if (x1 > xR) {
--   y1 = y0 + (xR - x0) * (y1 - y0) / (x1 - x0);
--   x1 = xR;
-- }
checkX2 :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkX2 (reflect, switched, bbox@(TypesGeography.BoundingBox _ _ maxX _), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | x2 > maxX = Just (reflect, switched, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY maxX newY2))
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newY2 = y1 + (maxX - x1) * (y2 - y1) / (x2 - x1)

-- if (y1 > yT) {
--   x1 = x0 + (yT - y0) * (x1 - x0) / (y1 - y0);
--   y1 = yT;
-- }
checkY2 :: (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine) -> Maybe (Bool, Bool, TypesGeography.BoundingBox, TypesGeography.GeoStorableLine)
checkY2 (reflect, switched, bbox@(TypesGeography.BoundingBox _ _ _ maxY), line@(TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)))
  | y2 > maxY = Just (reflect, switched, bbox, TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY newX2 maxY))
  | otherwise = Just (reflect, switched, bbox, line)
  where
    newX2 = x1 + (maxY - y1) * (x2 - x1) / (y2 - y1)
