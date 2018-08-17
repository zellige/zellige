{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.Line (
 clipLines
) where

import qualified Data.Vector               as DataVector
import qualified Data.Vector.Storable      as DataVectorStorable
import qualified Geography.VectorTile      as VectorTile
import qualified Geography.VectorTile      as VG
import           Prelude                   hiding (Left, Right, lines)

import           Data.Geometry.Types.Types

clipLines :: BoundingBoxPts -> DataVector.Vector VectorTile.LineString -> DataVector.Vector VectorTile.LineString
clipLines bb lines = DataVector.foldl' maybeAddLine mempty outCodes
  where
    outCodes = findOutCode bb lines

maybeAddLine :: DataVector.Vector VectorTile.LineString -> DataVectorStorable.Vector ((OutCode, VectorTile.Point), (OutCode, VectorTile.Point)) -> DataVector.Vector VectorTile.LineString
maybeAddLine acc pp =
  case (checkValidLineString . foldPointsToLine) pp of
    Just res -> DataVector.cons res acc
    Nothing  -> acc
  where
    foldPointsToLine = DataVectorStorable.foldr (mappend . (\((_,p1),(_,p2)) -> DataVectorStorable.fromList [p1, p2])) mempty
    checkValidLineString pts =
      if DataVectorStorable.length (segmentToLine pts) >= 2
        then Just (VectorTile.LineString (segmentToLine pts))
        else Nothing

findOutCode :: Functor f => BoundingBoxPts -> f VectorTile.LineString -> f (DataVectorStorable.Vector ((OutCode, VectorTile.Point), (OutCode, VectorTile.Point)))
findOutCode bb lines = fmap (DataVectorStorable.filter isSame . DataVectorStorable.map (evalDiffKeepSame bb)) (outCodeForLineStrings bb lines)


-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: DataVectorStorable.Vector VectorTile.Point -> DataVectorStorable.Vector VectorTile.Point
segmentToLine l = if DataVectorStorable.length l > 1 then DataVectorStorable.cons start (second l) else mempty
  where
    start = DataVectorStorable.head l
    second = DataVectorStorable.ifilter (\i _ -> odd i)

evalDiffKeepSame :: BoundingBoxPts -> ((OutCode, VG.Point), (OutCode, VG.Point)) -> ((OutCode, VG.Point), (OutCode, VG.Point))
evalDiffKeepSame bb (a@(o1, p1), b@(o2, p2)) =
  case compare o1 o2 of
    GT -> eval (clipAndCompute o1, b)
    LT -> eval (a, clipAndCompute o2)
    EQ -> (a, b)
  where
    eval = evalDiffKeepSame bb
    clipAndCompute o = computeNewOutCode $ clipPoint o bb p1 p2
    computeNewOutCode p = (computeOutCode bb p, p)

isSame :: ((OutCode, a), (OutCode, b)) -> Bool
isSame ((o1, _), (o2, _)) =
  case (o1, o2) of
    (Left   , Left  ) -> False
    (Right  , Right ) -> False
    (Bottom , Bottom) -> False
    (Top    , Top   ) -> False
    _                 -> True

clipPoint :: OutCode -> BoundingBoxPts -> VG.Point -> VG.Point -> VG.Point
clipPoint outCode BoundingBoxPts{_bbMinPts = (VG.Point minX minY), _bbMaxPts = (VG.Point maxX maxY)} (VG.Point x1 y1) (VG.Point x2 y2) =
  case outCode of
    Left   -> VectorTile.Point minX (y1 + (y2 - y1) * (minX - x1) `div` (x2 - x1))
    Right  -> VectorTile.Point maxX (y1 + (y2 - y1) * (maxX - x1) `div` (x2 - x1))
    Bottom -> VectorTile.Point (x1 + (x2 - x1) * (minY - y1) `div` (y2 - y1)) minY
    Top    -> VectorTile.Point (x1 + (x2 - x1) * (maxY - y1) `div` (y2 - y1)) maxY
    _      -> undefined

outCodeForLineStrings :: (Functor f) => BoundingBoxPts -> f VectorTile.LineString -> f (DataVectorStorable.Vector ((OutCode, VectorTile.Point), (OutCode, VectorTile.Point)))
outCodeForLineStrings bb = fmap $ DataVectorStorable.map out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints $ VectorTile.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DataVectorStorable.Vector VectorTile.Point -> DataVectorStorable.Vector (VectorTile.Point, VectorTile.Point)
linesFromPoints x = (DataVectorStorable.zipWith (,) <*> DataVectorStorable.tail) (DataVectorStorable.convert x)

outCodeForLine :: BoundingBoxPts -> VG.Point -> VG.Point -> ((OutCode, VG.Point), (OutCode, VG.Point))
outCodeForLine bb p1 p2 = (toP1, toP2)
  where
    toP1 = (computeOutCode bb p1, p1)
    toP2 = (computeOutCode bb p2, p2)

computeOutCode :: BoundingBoxPts -> VG.Point -> OutCode
computeOutCode BoundingBoxPts{_bbMinPts = (VG.Point minX minY), _bbMaxPts = (VG.Point maxX maxY)} (VG.Point x y)
  | y > maxY  = Top
  | y < minY  = Bottom
  | x > maxX  = Right
  | x < minX  = Left
  | otherwise = Inside
