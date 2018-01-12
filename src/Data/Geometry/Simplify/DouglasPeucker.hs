module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , perpendicularDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Vector as DV

type Point = (Double,Double)
type LineSegment = (Point,Point)

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt(((x1 - x2) ** 2) + ((y1 - y2) ** 2))

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
perpendicularDistance :: Point -> LineSegment -> Double
perpendicularDistance p@(pX, pY) (a@(aX, aY), b@(bX, bY))
    | a == b = distance p a
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (aX + u * deltaX, aY + u * deltaY)
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> DV.Vector Point -> DV.Vector Point
douglasPeucker epsilon points
  | points == DV.empty = DV.empty
  | dmax > epsilon = (douglasPeucker epsilon left) DV.++ (DV.tail (douglasPeucker epsilon right))
  | otherwise = (DV.snoc (DV.snoc DV.empty (DV.head points)) (DV.last points))
  where
      (left, right) = (DV.take index points, DV.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: DV.Vector Point -> (Double, Int)
splitAtMaxDistance points =
    DV.ifoldl' (\(accMax, index) ni a -> if cp a ls > accMax then (cp a ls, ni + 1) else (accMax, index)) (0.0, DV.length points) points
    where
        ls = (DV.head points, DV.last points)
        cp = perpendicularDistance


-- douglasPeucker 1.0 (fromList [(0.0,0.0),(1.0,0.1), (2.0,-0.1),(3.0,5.0), (4.0,6.0),(5.0,7.0), (6.0,8.1),(7.0,9.0), (8.0,9.0),(9.0,9.0)])
-- fromList [(0.0,0.0),(2.0,-0.1),(3.0,5.0),(7.0,9.0),(9.0,9.0)]
