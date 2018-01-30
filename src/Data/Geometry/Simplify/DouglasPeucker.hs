-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Vector          as DV
import qualified Geography.VectorTile as VG

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt(((x1 - x2) ** 2) + ((y1 - y2) ** 2))

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
shortestDistance :: (Double,Double) -> ((Double,Double),(Double,Double)) -> Double
shortestDistance p@(pX, pY) (a@(aX, aY), b@(bX, bY))
    | a == b = distance p a
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (aX + u * deltaX, aY + u * deltaY)
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> DV.Vector VG.Point -> DV.Vector VG.Point
douglasPeucker epsilon points
  | points == DV.empty = DV.empty
  | dmax > epsilon = (douglasPeucker epsilon left) DV.++ (DV.tail (douglasPeucker epsilon right))
  | otherwise = (DV.snoc (DV.snoc DV.empty (DV.head points)) (DV.last points))
  where
      (left, right) = (DV.take index points, DV.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: DV.Vector VG.Point -> (Double, Int)
splitAtMaxDistance points =
    DV.ifoldl' (\(accMax, index) ni a ->
        if cp (intTupleToDouble a) ls > accMax
            then (cp (intTupleToDouble a) ls, ni + 1)
             else (accMax, index)) (0.0, DV.length points) points
    where
        ls = (intTupleToDouble $ DV.head points, intTupleToDouble $DV.last points)
        cp = shortestDistance

intTupleToDouble :: (Num b, Integral a2, Integral a1) => (a1, a2) -> (Double, b)
intTupleToDouble (x, y) = (fromIntegral x :: Double, fromIntegral y)

