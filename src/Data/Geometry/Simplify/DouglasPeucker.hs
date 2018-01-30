-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Vector.Unboxed  as DVU
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
douglasPeucker :: Double -> DVU.Vector VG.Point -> DVU.Vector VG.Point
douglasPeucker epsilon points
  | points == DVU.empty = DVU.empty
  | dmax > epsilon = (douglasPeucker epsilon left) DVU.++ (DVU.tail (douglasPeucker epsilon right))
  | otherwise = (DVU.snoc (DVU.snoc DVU.empty (DVU.head points)) (DVU.last points))
  where
      (left, right) = (DVU.take index points, DVU.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: DVU.Vector VG.Point -> (Double, Int)
splitAtMaxDistance points =
    DVU.ifoldl' (\(accMax, index) ni a ->
        if cp (intTupleToDouble a) ls > accMax
            then (cp (intTupleToDouble a) ls, ni + 1)
             else (accMax, index)) (0.0, DVU.length points) points
    where
        ls = (intTupleToDouble $ DVU.head points, intTupleToDouble $DVU.last points)
        cp = shortestDistance

intTupleToDouble :: (Num b, Integral a2, Integral a1) => (a1, a2) -> (Double, b)
intTupleToDouble (x, y) = (fromIntegral x :: Double, fromIntegral y)

