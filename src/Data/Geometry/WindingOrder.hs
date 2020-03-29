{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.WindingOrder (
  surveyor
, isClockwise
, rewind
, ensureOrder
, WindingOrder(..)
) where

import qualified Data.Foldable   as Foldable
import qualified Data.Geospatial as Geospatial
import qualified Data.Sequence   as Sequence

data WindingOrder = Clockwise | AntiClockwise
   deriving (Eq, Show)

-- Surveyor's/Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
-- https://stackoverflow.com/questions/451426/how-do-i-calculate-the-area-of-a-2d-polygon
-- http://geomalgorithms.com/a01-_area.html - area2D_Polygon()
-- https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area#Haskell
-- [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6), (3,4)] -> -30
surveyor :: Sequence.Seq Geospatial.PointXY -> Double
surveyor v = (/ 2) . Foldable.foldl' (+) 0 $ Sequence.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where
    -- [3,5,12,9,5,3]
    xs = fmap Geospatial._xyX v
    -- [4,11,8,5,6,4] -> [11,8,5,6,4,4]
    yns = case v of
            (hV' Sequence.:<| restV') -> fmap Geospatial._xyY (restV' Sequence.|> hV')
            _ -> Sequence.empty
    -- [4,11,8,5,6,4] -> [4,4,11,8,5,6]
    yps = case v of
            (restV' Sequence.:|> hV') -> fmap Geospatial._xyY (hV' Sequence.<| restV')
            _ -> Sequence.empty
{-# INLINE surveyor #-}

isClockwise :: Sequence.Seq Geospatial.PointXY -> Bool
isClockwise v = surveyor v <= 0

rewind :: Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
rewind v =
    case v of
      (firstPt Sequence.:<| (middle Sequence.:|> lastPt)) -> (firstPt Sequence.<| Sequence.reverse middle) Sequence.|> lastPt
      _                                                   -> v

ensureOrder :: WindingOrder -> Sequence.Seq Geospatial.PointXY ->  Sequence.Seq Geospatial.PointXY
ensureOrder order v =
  case order of
    Clockwise     -> if isClockwise v then v else rewind v
    AntiClockwise -> if isClockwise v then rewind v else v

