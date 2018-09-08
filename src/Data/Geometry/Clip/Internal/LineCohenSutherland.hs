{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.LineCohenSutherland (
  clipLinesCs
, newClipLinesCs
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LineString                  as LineString
import qualified Data.Validation                  as Validation
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile
import           Prelude                          hiding (Left, Right, lines)

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

clipLinesCs :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLinesCs bb lines = Vector.foldl' maybeAddLine mempty (findClipLines bb lines)

newClipLinesCs :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
newClipLinesCs bb lines (Geospatial.GeoFeature bbox _ props fId) = Vector.cons reMakeFeature
  where
    reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine multiLine)) props fId
    multiLine = Vector.foldl' newMaybeAddLine mempty (newFindClipLines bb (Geospatial.splitGeoMultiLine lines))

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector TypesGeography.ClipLine -> Vector.Vector VectorTile.LineString
maybeAddLine acc pp =
  case (ClipLine.checkValidLineString . foldPointsToLine) pp of
    Just res -> Vector.cons res acc
    Nothing  -> acc

newMaybeAddLine :: Vector.Vector (LineString.LineString Geospatial.GeoPositionWithoutCRS) -> Vector.Vector TypesGeography.GeoClipLine -> Vector.Vector (LineString.LineString Geospatial.GeoPositionWithoutCRS)
newMaybeAddLine acc pp =
  case (LineString.fromVector . newFoldPointsToLine) pp of
    Validation.Success res -> Vector.cons res acc
    Validation.Failure _   -> acc

foldPointsToLine :: VectorStorable.Vector TypesGeography.ClipLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.ClipLine (TypesGeography.ClipPoint _ p1) (TypesGeography.ClipPoint _ p2)) -> VectorStorable.fromList [p1, p2])) mempty

newFoldPointsToLine :: Vector.Vector TypesGeography.GeoClipLine -> Vector.Vector Geospatial.GeoPositionWithoutCRS
newFoldPointsToLine = Vector.foldr (mappend . (\(TypesGeography.GeoClipLine (TypesGeography.GeoClipPoint _ p1) (TypesGeography.GeoClipPoint _ p2)) -> Vector.fromList [Geospatial.GeoPointXY p1, Geospatial.GeoPointXY p2])) mempty

findClipLines :: Functor f => TypesGeography.BoundingBoxPts -> f VectorTile.LineString -> f (VectorStorable.Vector TypesGeography.ClipLine)
findClipLines bb lines = fmap (VectorStorable.filter isSame . VectorStorable.map (evalDiffKeepSame bb)) (outCodeForLineStrings bb lines)

newFindClipLines :: Functor f => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Vector.Vector TypesGeography.GeoClipLine)
newFindClipLines bb lines = fmap (Vector.filter newIsSame . Vector.map (newEvalDiffKeepSame bb)) (newOutCodeForLineStrings bb lines)

evalDiffKeepSame :: TypesGeography.BoundingBoxPts -> TypesGeography.ClipLine -> TypesGeography.ClipLine
evalDiffKeepSame bb (TypesGeography.ClipLine a@(TypesGeography.ClipPoint o1 p1) b@(TypesGeography.ClipPoint o2 p2)) =
  case compare o1 o2 of
    GT -> eval $ TypesGeography.ClipLine (clipAndCompute o1) b
    LT -> eval $ TypesGeography.ClipLine a (clipAndCompute o2)
    EQ -> TypesGeography.ClipLine a b
  where
    eval = evalDiffKeepSame bb
    clipAndCompute o = computeNewOutCode $ clipPoint o bb p1 p2
    computeNewOutCode p = TypesGeography.ClipPoint (computeOutCode bb p) p

newEvalDiffKeepSame ::  TypesGeography.BoundingBox -> TypesGeography.GeoClipLine -> TypesGeography.GeoClipLine
newEvalDiffKeepSame bb (TypesGeography.GeoClipLine a@(TypesGeography.GeoClipPoint o1 p1) b@(TypesGeography.GeoClipPoint o2 p2)) =
  case compare o1 o2 of
    GT -> eval $ TypesGeography.GeoClipLine (clipAndCompute o1) b
    LT -> eval $ TypesGeography.GeoClipLine a (clipAndCompute o2)
    EQ -> TypesGeography.GeoClipLine a b
  where
    eval = newEvalDiffKeepSame bb
    clipAndCompute o = computeNewOutCode $ newClipPoint o bb p1 p2
    computeNewOutCode p = TypesGeography.GeoClipPoint (newComputeOutCode bb p) p

isSame :: TypesGeography.ClipLine -> Bool
isSame (TypesGeography.ClipLine (TypesGeography.ClipPoint o1 _) (TypesGeography.ClipPoint o2 _)) =
  case (o1, o2) of
    (TypesGeography.Left   , TypesGeography.Left  ) -> False
    (TypesGeography.Right  , TypesGeography.Right ) -> False
    (TypesGeography.Bottom , TypesGeography.Bottom) -> False
    (TypesGeography.Top    , TypesGeography.Top   ) -> False
    _                                               -> True

newIsSame :: TypesGeography.GeoClipLine -> Bool
newIsSame (TypesGeography.GeoClipLine (TypesGeography.GeoClipPoint o1 _) (TypesGeography.GeoClipPoint o2 _)) =
  case (o1, o2) of
    (TypesGeography.Left   , TypesGeography.Left  ) -> False
    (TypesGeography.Right  , TypesGeography.Right ) -> False
    (TypesGeography.Bottom , TypesGeography.Bottom) -> False
    (TypesGeography.Top    , TypesGeography.Top   ) -> False
    _                                               -> True

clipPoint :: TypesGeography.OutCode -> TypesGeography.BoundingBoxPts -> VectorTile.Point -> VectorTile.Point -> VectorTile.Point
clipPoint outCode TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VectorTile.Point minX minY), TypesGeography._bbMaxPts = (VectorTile.Point maxX maxY)} (VectorTile.Point x1 y1) (VectorTile.Point x2 y2) =
  case outCode of
    TypesGeography.Left   -> VectorTile.Point minX (y1 + (y2 - y1) * (minX - x1) `div` (x2 - x1))
    TypesGeography.Right  -> VectorTile.Point maxX (y1 + (y2 - y1) * (maxX - x1) `div` (x2 - x1))
    TypesGeography.Bottom -> VectorTile.Point (x1 + (x2 - x1) * (minY - y1) `div` (y2 - y1)) minY
    TypesGeography.Top    -> VectorTile.Point (x1 + (x2 - x1) * (maxY - y1) `div` (y2 - y1)) maxY
    _      -> undefined

newClipPoint :: TypesGeography.OutCode -> TypesGeography.BoundingBox -> Geospatial.PointXY -> Geospatial.PointXY -> Geospatial.PointXY
newClipPoint outCode (TypesGeography.BoundingBox minX minY maxX maxY) (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2) =
  case outCode of
    TypesGeography.Left   -> Geospatial.PointXY minX (y1 + (y2 - y1) * (minX - x1) / (x2 - x1))
    TypesGeography.Right  -> Geospatial.PointXY maxX (y1 + (y2 - y1) * (maxX - x1) / (x2 - x1))
    TypesGeography.Bottom -> Geospatial.PointXY (x1 + (x2 - x1) * (minY - y1) / (y2 - y1)) minY
    TypesGeography.Top    -> Geospatial.PointXY (x1 + (x2 - x1) * (maxY - y1) / (y2 - y1)) maxY
    _      -> undefined

outCodeForLineStrings :: (Functor f) => TypesGeography.BoundingBoxPts -> f VectorTile.LineString -> f (VectorStorable.Vector TypesGeography.ClipLine)
outCodeForLineStrings bb = fmap $ VectorStorable.map out . ClipLine.getLines
  where
    out = outCodeForLine bb

newOutCodeForLineStrings :: (Functor f) => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Vector.Vector TypesGeography.GeoClipLine)
newOutCodeForLineStrings bb = fmap $ Vector.map out . ClipLine.newGetLines
    where
      out = newOutCodeForLine bb

outCodeForLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> TypesGeography.ClipLine
outCodeForLine bb (TypesGeography.StorableLine p1 p2) = TypesGeography.ClipLine toP1 toP2
  where
    toP1 = TypesGeography.ClipPoint (computeOutCode bb p1) p1
    toP2 = TypesGeography.ClipPoint (computeOutCode bb p2) p2

newOutCodeForLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> TypesGeography.GeoClipLine
newOutCodeForLine bb (TypesGeography.GeoStorableLine p1 p2) = TypesGeography.GeoClipLine toP1 toP2
  where
    toP1 = TypesGeography.GeoClipPoint (newComputeOutCode bb p1) p1
    toP2 = TypesGeography.GeoClipPoint (newComputeOutCode bb p2) p2

computeOutCode :: TypesGeography.BoundingBoxPts -> VectorTile.Point -> TypesGeography.OutCode
computeOutCode TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VectorTile.Point minX minY), TypesGeography._bbMaxPts = (VectorTile.Point maxX maxY)} (VectorTile.Point x y)
  | y > maxY  = TypesGeography.Top
  | y < minY  = TypesGeography.Bottom
  | x > maxX  = TypesGeography.Right
  | x < minX  = TypesGeography.Left
  | otherwise = TypesGeography.Inside
  | x < minX  = TypesGeography.Left
  | otherwise = TypesGeography.Inside

newComputeOutCode :: TypesGeography.BoundingBox -> Geospatial.PointXY -> TypesGeography.OutCode
newComputeOutCode (TypesGeography.BoundingBox minX minY maxX maxY) (Geospatial.PointXY x y)
  | y > maxY  = TypesGeography.Top
  | y < minY  = TypesGeography.Bottom
  | x > maxX  = TypesGeography.Right
  | x < minX  = TypesGeography.Left
  | otherwise = TypesGeography.Inside
