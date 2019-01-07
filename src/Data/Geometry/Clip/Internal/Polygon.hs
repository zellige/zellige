{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.Clip.Internal.Polygon (
  closeIfNot
, surveyor
) where

import qualified Data.Foldable   as Foldable
import qualified Data.Geospatial as Geospatial
import qualified Data.Sequence   as Sequence

closeIfNot :: Sequence.Seq Geospatial.PointXY -> Maybe (Sequence.Seq Geospatial.PointXY)
closeIfNot poly =
  case poly of
    (firstPt Sequence.:<| (_ Sequence.:|> lastPt)) ->
      if lastPt /= firstPt
        then Just $ lastPt Sequence.<| poly
        else Just poly
    _ -> Nothing
{-# INLINE closeIfNot #-}

-- Surveyor's/Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
-- https://stackoverflow.com/questions/451426/how-do-i-calculate-the-area-of-a-2d-polygon
-- https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area#Haskell
-- [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6)] -> -30
surveyor :: Sequence.Seq Geospatial.PointXY -> Double
surveyor v = (/ 2) . Foldable.foldl' (+) 0 $ Sequence.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where
    -- [3,5,12,9,5]
    xs = fmap Geospatial._xyX v
    -- [4,11,8,5,6] -> [11,8,5,6,4]
    yns = case v of
            (hV' Sequence.:<| restV') -> fmap Geospatial._xyY (restV' Sequence.|> hV')
            _ -> Sequence.empty
    -- [4,11,8,5,6] -> [6,4,11,8,5]
    yps = case v of
            (restV' Sequence.:|> hV') -> fmap Geospatial._xyY (hV' Sequence.<| restV')
            _ -> Sequence.empty
{-# INLINE surveyor #-}
