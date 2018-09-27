-- Douglas Peucker using Shortest Distance
module Data.Geometry.Simplify.DouglasPeucker
    ( distance
    , shortestDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Sequence                 as Sequence

import qualified Data.Geometry.Types.Geography as TypesGeography
import           Prelude                       hiding (last, tail)

type Distance = Double
type Index = Int

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
douglasPeucker epsilon points
  | points == Sequence.empty = Sequence.empty
  | dMax > epsilon = douglasPeucker epsilon left Sequence.>< tail (douglasPeucker epsilon right)
  | otherwise = firstAndLastPoint points
  where
    (left, right) = (Sequence.take index points, Sequence.drop (index - 1) points)
    (dMax, index) = splitAtMaxDistance points

splitAtMaxDistance :: Sequence.Seq Geospatial.PointXY -> (Distance, Index)
splitAtMaxDistance points@((first Sequence.:<| _) Sequence.:|> last) =
  Sequence.foldlWithIndex (\(accMax, index) i point ->
    if shortestDistance point lineSegment > accMax
      then (shortestDistance point lineSegment, i + 1)
      else (accMax, index)
  ) (0.0, Sequence.length points) points
  where
    lineSegment = TypesGeography.GeoStorableLine first last
splitAtMaxDistance _ = (0.0, 0)

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
shortestDistance :: Geospatial.PointXY -> TypesGeography.GeoStorableLine -> Distance
shortestDistance p@(Geospatial.PointXY pX pY) (TypesGeography.GeoStorableLine a@(Geospatial.PointXY aX aY) b@(Geospatial.PointXY bX bY))
    | a == b = distance p a
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (Geospatial.PointXY (aX + u * deltaX) (aY + u * deltaY))
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

distance :: Geospatial.PointXY -> Geospatial.PointXY -> Distance
distance (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2) = sqrt(((x1 - x2) ** 2) + ((y1 - y2) ** 2))

firstAndLastPoint :: Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
firstAndLastPoint ((first Sequence.:<| _) Sequence.:|> last) = Sequence.fromList [first, last]
firstAndLastPoint _ = Sequence.empty

tail :: Sequence.Seq a -> Sequence.Seq a
tail s =
  case Sequence.viewl s of
    Sequence.EmptyL    -> Sequence.empty
    (_ Sequence.:< xs) -> xs
