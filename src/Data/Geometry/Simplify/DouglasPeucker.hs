-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    , seqDouglasPeucker
    ) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Sequence                 as Sequence
import qualified Data.Vector.Storable          as VectorStorable

import qualified Data.Geometry.Types.Geography as TypesGeography

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
seqDouglasPeucker :: Double -> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
seqDouglasPeucker epsilon points
  | points == Sequence.empty = Sequence.empty
  | dMax > epsilon = seqDouglasPeucker epsilon left Sequence.>< seqTail (seqDouglasPeucker epsilon right)
  | otherwise = Sequence.fromList [(seqHead points), (seqLast points)]
  where
    (left, right) = (Sequence.take index points, Sequence.drop (index - 1) points)
    (dMax, index) = seqSplitAtMaxDistance points

seqSplitAtMaxDistance :: Sequence.Seq Geospatial.PointXY -> (Distance, Index)
seqSplitAtMaxDistance points =
  Sequence.foldlWithIndex (\(accMax, index) ni a ->
    if cp a ls > accMax
      then (cp a ls, ni + 1)
      else (accMax, index)
  ) (0.0, Sequence.length points) points
  where
    ls = TypesGeography.GeoStorableLine (seqHead points) (seqLast points)
    cp = shortestDistance

seqHead :: Sequence.Seq a -> a
seqHead s = Sequence.index s 0

seqLast :: Sequence.Seq a -> a
seqLast s = Sequence.index s ((Sequence.length s) - 1)

seqTail :: Sequence.Seq a -> Sequence.Seq a
seqTail s =
  case Sequence.viewl s of
    Sequence.EmptyL    -> Sequence.empty
    (_ Sequence.:< xs) -> xs

type Distance = Double
type Index = Int

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

