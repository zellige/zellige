-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable

import qualified Data.Geometry.Types.Geography as TypesGeography

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> VectorStorable.Vector Geospatial.PointXY -> VectorStorable.Vector Geospatial.PointXY
douglasPeucker epsilon points
  | points == VectorStorable.empty = VectorStorable.empty
  | dmax > epsilon = douglasPeucker epsilon left VectorStorable.++ VectorStorable.tail (douglasPeucker epsilon right)
  | otherwise = VectorStorable.snoc (VectorStorable.singleton (VectorStorable.head points)) (VectorStorable.last points)
  where
      (left, right) = (VectorStorable.take index points, VectorStorable.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: VectorStorable.Vector Geospatial.PointXY -> (Double, Int)
splitAtMaxDistance points =
  VectorStorable.ifoldl' (\(accMax, index) ni a ->
        if cp a ls > accMax
            then (cp a ls, ni + 1)
              else (accMax, index)) (0.0, VectorStorable.length points) points
    where
        ls = TypesGeography.GeoStorableLine (VectorStorable.head points) (VectorStorable.last points)
        cp = shortestDistance

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
shortestDistance :: Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Double
shortestDistance p@(Geospatial.PointXY pX pY) (TypesGeography.GeoStorableLine a@(Geospatial.PointXY aX aY) b@(Geospatial.PointXY bX bY))
    | a == b = distance p a
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (Geospatial.PointXY (aX + u * deltaX) (aY + u * deltaY))
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

distance :: Geospatial.PointXY -> Geospatial.PointXY -> Double
distance (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2) = sqrt(((x1 - x2) ** 2) + ((y1 - y2) ** 2))

