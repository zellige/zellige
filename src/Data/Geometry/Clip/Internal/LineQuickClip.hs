{-# LANGUAGE FlexibleContexts #-}

-- QuickClip line clipping
-- https://ieeexplore.ieee.org/document/694214/

module Data.Geometry.Clip.Internal.LineQuickClip (
  clipLineQc
  , clipLinesQc
  , clipOrDiscard
  , clipLineQcMap
  , clipLinesQcMap
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Foldable                    as Foldable
import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LineString                  as LineString
import qualified Data.Sequence                    as Sequence
import qualified Data.Validation                  as Validation
import           Prelude                          hiding (lines)

clipLineQc :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipLineQc bb line feature acc =
  case clipLineQcMap bb line of
    Just res -> (Sequence.<|) (Geospatial.reWrapGeometry feature (Geospatial.Line res)) acc
    Nothing  -> acc

clipLineQcMap :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Maybe Geospatial.GeoLine
clipLineQcMap bb line =
  case LineString.fromSeq clippedLine of
    Validation.Success res -> Just (Geospatial.GeoLine res)
    Validation.Failure _   -> Nothing
  where
    clippedLine = ClipLine.lineToGeoPoint $ lineToClippedPoints bb line

clipLinesQc :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipLinesQc bb lines (Geospatial.GeoFeature bbox _ props fId) acc =
  case clipLinesQcMap bb lines of
    Just res -> (Sequence.<|) (Geospatial.GeoFeature bbox (Geospatial.MultiLine res) props fId) acc
    Nothing  -> acc

clipLinesQcMap :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Maybe Geospatial.GeoMultiLine
clipLinesQcMap bb lines =
  if Sequence.null multiLine then Nothing else Just (Geospatial.GeoMultiLine multiLine)
  where
    multiLine = Foldable.foldl' maybeAddLine mempty (linesToClippedPoints bb (Geospatial.splitGeoMultiLine lines))

maybeAddLine :: Sequence.Seq
               (LineString.LineString Geospatial.GeoPositionWithoutCRS)
             -> Sequence.Seq TypesGeography.GeoStorableLine
             -> Sequence.Seq
                  (LineString.LineString Geospatial.GeoPositionWithoutCRS)
maybeAddLine acc pp =
  case clipLineToValidationLineString pp of
    Validation.Success res -> (Sequence.<|) res acc
    Validation.Failure _   -> acc

clipLineToValidationLineString :: Sequence.Seq TypesGeography.GeoStorableLine -> Validation.Validation LineString.VectorToLineStringError (LineString.LineString Geospatial.GeoPositionWithoutCRS)
clipLineToValidationLineString lines = LineString.fromSeq (ClipLine.lineToGeoPoint lines)

lineToClippedPoints :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Sequence.Seq TypesGeography.GeoStorableLine
lineToClippedPoints bb geoLine = Foldable.foldr (clipOrDiscard bb) Sequence.empty (ClipLine.getLines geoLine)

linesToClippedPoints :: Functor f => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Sequence.Seq TypesGeography.GeoStorableLine)
linesToClippedPoints bb = fmap (lineToClippedPoints bb)

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> Sequence.Seq TypesGeography.GeoStorableLine -> Sequence.Seq TypesGeography.GeoStorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> (Sequence.<|) clippedLine acc

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
