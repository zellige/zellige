{-# LANGUAGE FlexibleContexts #-}

-- Cohen Sutherland Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.CohenSutherlandLine (
 clipLines
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile
import qualified Geography.VectorTile             as VG
import           Prelude                          hiding (Left, Right, lines)

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

clipLines :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLines bb lines = Vector.foldl' maybeAddLine mempty outCodes
  where
    outCodes = findOutCode bb lines

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector TypesGeography.ClipLine -> Vector.Vector VectorTile.LineString
maybeAddLine acc pp =
  case (ClipLine.checkValidLineString . foldPointsToLine) pp of
    Just res -> Vector.cons res acc
    Nothing  -> acc
  where
    foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.ClipLine (TypesGeography.ClipPoint _ p1) (TypesGeography.ClipPoint _ p2)) -> VectorStorable.fromList [p1, p2])) mempty

findOutCode :: Functor f => TypesGeography.BoundingBoxPts -> f VectorTile.LineString -> f (VectorStorable.Vector TypesGeography.ClipLine)
findOutCode bb lines = fmap (VectorStorable.filter isSame . VectorStorable.map (evalDiffKeepSame bb)) (outCodeForLineStrings bb lines)

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

isSame :: TypesGeography.ClipLine -> Bool
isSame (TypesGeography.ClipLine (TypesGeography.ClipPoint o1 _) (TypesGeography.ClipPoint o2 _)) =
  case (o1, o2) of
    (TypesGeography.Left   , TypesGeography.Left  ) -> False
    (TypesGeography.Right  , TypesGeography.Right ) -> False
    (TypesGeography.Bottom , TypesGeography.Bottom) -> False
    (TypesGeography.Top    , TypesGeography.Top   ) -> False
    _                                               -> True

clipPoint :: TypesGeography.OutCode -> TypesGeography.BoundingBoxPts -> VG.Point -> VG.Point -> VG.Point
clipPoint outCode TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VG.Point minX minY), TypesGeography._bbMaxPts = (VG.Point maxX maxY)} (VG.Point x1 y1) (VG.Point x2 y2) =
  case outCode of
    TypesGeography.Left   -> VectorTile.Point minX (y1 + (y2 - y1) * (minX - x1) `div` (x2 - x1))
    TypesGeography.Right  -> VectorTile.Point maxX (y1 + (y2 - y1) * (maxX - x1) `div` (x2 - x1))
    TypesGeography.Bottom -> VectorTile.Point (x1 + (x2 - x1) * (minY - y1) `div` (y2 - y1)) minY
    TypesGeography.Top    -> VectorTile.Point (x1 + (x2 - x1) * (maxY - y1) `div` (y2 - y1)) maxY
    _      -> undefined

outCodeForLineStrings :: (Functor f) => TypesGeography.BoundingBoxPts -> f VectorTile.LineString -> f (VectorStorable.Vector TypesGeography.ClipLine)
outCodeForLineStrings bb = fmap $ VectorStorable.map out . getLines
  where
    out = outCodeForLine bb
    getLines line = linesFromPoints $ VectorTile.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector TypesGeography.StorableLine
linesFromPoints x = (VectorStorable.zipWith TypesGeography.StorableLine <*> VectorStorable.tail) (VectorStorable.convert x)

outCodeForLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> TypesGeography.ClipLine
outCodeForLine bb (TypesGeography.StorableLine p1 p2) = TypesGeography.ClipLine toP1 toP2
  where
    toP1 = TypesGeography.ClipPoint (computeOutCode bb p1) p1
    toP2 = TypesGeography.ClipPoint (computeOutCode bb p2) p2

computeOutCode :: TypesGeography.BoundingBoxPts -> VG.Point -> TypesGeography.OutCode
computeOutCode TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VG.Point minX minY), TypesGeography._bbMaxPts = (VG.Point maxX maxY)} (VG.Point x y)
  | y > maxY  = TypesGeography.Top
  | y < minY  = TypesGeography.Bottom
  | x > maxX  = TypesGeography.Right
  | x < minX  = TypesGeography.Left
  | otherwise = TypesGeography.Inside