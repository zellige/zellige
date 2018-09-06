-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Vector.Storable as VectorStorable
import qualified Geography.VectorTile as VectorTile

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
douglasPeucker :: Double -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
douglasPeucker epsilon points
  | points == VectorStorable.empty = VectorStorable.empty
  | dmax > epsilon = douglasPeucker epsilon left VectorStorable.++ VectorStorable.tail (douglasPeucker epsilon right)
  | otherwise = VectorStorable.snoc (VectorStorable.singleton (VectorStorable.head points)) (VectorStorable.last points)
  where
      (left, right) = (VectorStorable.take index points, VectorStorable.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: VectorStorable.Vector VectorTile.Point -> (Double, Int)
splitAtMaxDistance points =
  VectorStorable.ifoldl' (\(accMax, index) ni a ->
        if cp (intTupleToDouble a) ls > accMax
            then (cp (intTupleToDouble a) ls, ni + 1)
             else (accMax, index)) (0.0, VectorStorable.length points) points
    where
        ls = (intTupleToDouble $ VectorStorable.head points, intTupleToDouble $ VectorStorable.last points)
        cp = shortestDistance

intTupleToDouble :: (Num b) => VectorTile.Point -> (Double, b)
intTupleToDouble (VectorTile.Point x y) = (fromIntegral x :: Double, fromIntegral y)
