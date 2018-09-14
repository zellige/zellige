-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Vector                   as Vector

import qualified Data.Geometry.Types.Geography as TypesGeography

distance :: Geospatial.PointXY -> Geospatial.PointXY -> Double
distance (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2) = sqrt(((x1 - x2) ** 2) + ((y1 - y2) ** 2))

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

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
douglasPeucker epsilon points
  | points == Vector.empty = Vector.empty
  | dmax > epsilon = douglasPeucker epsilon left Vector.++ Vector.tail (douglasPeucker epsilon right)
  | otherwise = Vector.snoc (Vector.singleton (Vector.head points)) (Vector.last points)
  where
      (left, right) = (Vector.take index points, Vector.drop (index - 1) points)
      (dmax, index) = splitAtMaxDistance points

splitAtMaxDistance :: Vector.Vector Geospatial.PointXY -> (Double, Int)
splitAtMaxDistance points =
  Vector.ifoldl' (\(accMax, index) ni a ->
        if cp a ls > accMax
            then (cp a ls, ni + 1)
             else (accMax, index)) (0.0, Vector.length points) points
    where
        ls = TypesGeography.GeoStorableLine (Vector.head points) (Vector.last points)
        cp = shortestDistance
