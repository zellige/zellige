{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip.Internal.Line (
 clipLines
) where

import qualified Data.Foldable             as DF
import qualified Data.Sequence             as DS
import qualified Data.Vector.Unboxed       as DVU
import qualified Geography.VectorTile      as VG
import           Prelude                   hiding (Left, Right, lines)

import           Data.Geometry.Types.Types

clipLines :: BoundingBoxPts -> DS.Seq VG.LineString -> DS.Seq VG.LineString
clipLines bb lines = DF.foldl' maybeAddLine mempty outCodes
  where
    outCodes = findOutCode bb lines

maybeAddLine :: DS.Seq VG.LineString -> DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)) -> DS.Seq VG.LineString
maybeAddLine acc pp =
  case (checkValidLineString . foldPointsToLine) pp of
    Just res -> res DS.<| acc
    Nothing  -> acc
  where
    foldPointsToLine = DVU.foldr (mappend . (\((_,p1),(_,p2)) -> DVU.fromList [p1, p2])) mempty
    checkValidLineString pts =
      if DVU.length (segmentToLine pts) >= 2
        then Just (VG.LineString (segmentToLine pts))
        else Nothing

findOutCode :: Functor f => BoundingBoxPts -> f VG.LineString -> f (DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
findOutCode bb lines = fmap (DVU.filter isSame . DVU.map (evalDiffKeepSame bb)) (outCodeForLineStrings bb lines)

-- Remove duplicate points in segments [(1,2),(2,3)] becomes [1,2,3]
segmentToLine :: DVU.Vector VG.Point -> DVU.Vector VG.Point
segmentToLine l = if DVU.length l > 1 then DVU.cons start (second l) else mempty
  where
    start = DVU.head l
    second = DVU.ifilter (\i _ -> odd i)

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
clipPoint outCode BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x1, y1) (x2, y2) =
  case outCode of
    Left   -> (minX, y1 + (y2 - y1) * (minX - x1) `div` (x2 - x1))
    Right  -> (maxX, y1 + (y2 - y1) * (maxX - x1) `div` (x2 - x1))
    Bottom -> (x1 + (x2 - x1) * (minY - y1) `div` (y2 - y1), minY)
    Top    -> (x1 + (x2 - x1) * (maxY - y1) `div` (y2 - y1), maxY)
    _      -> undefined

outCodeForLineStrings :: (Functor f) => BoundingBoxPts -> f VG.LineString -> f (DVU.Vector ((OutCode, VG.Point), (OutCode, VG.Point)))
outCodeForLineStrings bb = fmap $ DVU.map out . getLines
  where
    out = uncurry (outCodeForLine bb)
    getLines line = linesFromPoints $ VG.lsPoints line

-- Create segments from points [1,2,3] becomes [(1,2),(2,3)]
linesFromPoints :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
linesFromPoints x = (DVU.zip <*> DVU.tail) (DVU.convert x)

outCodeForLine :: BoundingBoxPts -> VG.Point -> VG.Point -> ((OutCode, VG.Point), (OutCode, VG.Point))
outCodeForLine bb p1 p2 = (toP1, toP2)
  where
    toP1 = (computeOutCode bb p1, p1)
    toP2 = (computeOutCode bb p2, p2)

computeOutCode :: BoundingBoxPts -> VG.Point -> OutCode
computeOutCode BoundingBoxPts{_bbMinPts = (minX, minY), _bbMaxPts = (maxX, maxY)} (x,y)
  | y > maxY  = Top
  | y < minY  = Bottom
  | x > maxX  = Right
  | x < minX  = Left
  | otherwise = Inside
