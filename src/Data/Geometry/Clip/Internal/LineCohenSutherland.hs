{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.LineCohenSutherland
( newClipLinesCs
, newClipLineCs
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LineString                  as LineString
import qualified Data.Validation                  as Validation
import qualified Data.Vector                      as Vector
import           Prelude                          hiding (Left, Right, lines)

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

newClipLineCs :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
newClipLineCs _ _ (Geospatial.GeoFeature bbox _ props fId) = Vector.cons reMakeFeature
  where
    reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine line)) props fId
    line = undefined

newClipLinesCs :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
newClipLinesCs bb lines (Geospatial.GeoFeature bbox _ props fId) = Vector.cons reMakeFeature
  where
    reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine multiLine)) props fId
    multiLine = Vector.foldl' newMaybeAddLine mempty (newFindClipLines bb (Geospatial.splitGeoMultiLine lines))

newMaybeAddLine :: Vector.Vector (LineString.LineString Geospatial.GeoPositionWithoutCRS) -> Vector.Vector TypesGeography.GeoClipLine -> Vector.Vector (LineString.LineString Geospatial.GeoPositionWithoutCRS)
newMaybeAddLine acc pp =
  case (LineString.fromVector . newFoldPointsToLine) pp of
    Validation.Success res -> Vector.cons res acc
    Validation.Failure _   -> acc

newFoldPointsToLine :: Vector.Vector TypesGeography.GeoClipLine -> Vector.Vector Geospatial.GeoPositionWithoutCRS
newFoldPointsToLine = Vector.foldr (mappend . (\(TypesGeography.GeoClipLine (TypesGeography.GeoClipPoint _ p1) (TypesGeography.GeoClipPoint _ p2)) -> Vector.fromList [Geospatial.GeoPointXY p1, Geospatial.GeoPointXY p2])) mempty

newFindClipLines :: Functor f => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Vector.Vector TypesGeography.GeoClipLine)
newFindClipLines bb lines = fmap (Vector.filter newIsSame . Vector.map (newEvalDiffKeepSame bb)) (newOutCodeForLineStrings bb lines)

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

newIsSame :: TypesGeography.GeoClipLine -> Bool
newIsSame (TypesGeography.GeoClipLine (TypesGeography.GeoClipPoint o1 _) (TypesGeography.GeoClipPoint o2 _)) =
  case (o1, o2) of
    (TypesGeography.Left   , TypesGeography.Left  ) -> False
    (TypesGeography.Right  , TypesGeography.Right ) -> False
    (TypesGeography.Bottom , TypesGeography.Bottom) -> False
    (TypesGeography.Top    , TypesGeography.Top   ) -> False
    _                                               -> True

newClipPoint :: TypesGeography.OutCode -> TypesGeography.BoundingBox -> Geospatial.PointXY -> Geospatial.PointXY -> Geospatial.PointXY
newClipPoint outCode (TypesGeography.BoundingBox minX minY maxX maxY) (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2) =
  case outCode of
    TypesGeography.Left   -> Geospatial.PointXY minX (y1 + (y2 - y1) * (minX - x1) / (x2 - x1))
    TypesGeography.Right  -> Geospatial.PointXY maxX (y1 + (y2 - y1) * (maxX - x1) / (x2 - x1))
    TypesGeography.Bottom -> Geospatial.PointXY (x1 + (x2 - x1) * (minY - y1) / (y2 - y1)) minY
    TypesGeography.Top    -> Geospatial.PointXY (x1 + (x2 - x1) * (maxY - y1) / (y2 - y1)) maxY
    _      -> undefined

newOutCodeForLineStrings :: (Functor f) => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Vector.Vector TypesGeography.GeoClipLine)
newOutCodeForLineStrings bb = fmap $ Vector.map out . ClipLine.newGetLines
    where
      out = newOutCodeForLine bb

newOutCodeForLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> TypesGeography.GeoClipLine
newOutCodeForLine bb (TypesGeography.GeoStorableLine p1 p2) = TypesGeography.GeoClipLine toP1 toP2
  where
    toP1 = TypesGeography.GeoClipPoint (newComputeOutCode bb p1) p1
    toP2 = TypesGeography.GeoClipPoint (newComputeOutCode bb p2) p2

newComputeOutCode :: TypesGeography.BoundingBox -> Geospatial.PointXY -> TypesGeography.OutCode
newComputeOutCode (TypesGeography.BoundingBox minX minY maxX maxY) (Geospatial.PointXY x y)
  | y > maxY  = TypesGeography.Top
  | y < minY  = TypesGeography.Bottom
  | x > maxX  = TypesGeography.Right
  | x < minX  = TypesGeography.Left
  | otherwise = TypesGeography.Inside
