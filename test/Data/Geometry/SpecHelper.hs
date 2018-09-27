module Data.Geometry.SpecHelper where

import qualified Data.Geospatial      as Geospatial
import qualified Data.LinearRing      as LinearRing
import qualified Data.LineString      as LineString
import qualified Data.Sequence        as Sequence
import qualified Geography.VectorTile as VectorTile

tupleToPts :: [(Int, Int)] -> Sequence.Seq VectorTile.Point
tupleToPts = foldr (\(x,y) acc -> VectorTile.Point x y Sequence.<| acc) Sequence.empty

mkLineString :: (Double, Double) -> (Double, Double) -> [(Double, Double)] -> LineString.LineString Geospatial.GeoPositionWithoutCRS
mkLineString p1 p2 rest = LineString.makeLineString (tupleToGeoPts p1) (tupleToGeoPts p2) (Sequence.fromList $ fmap tupleToGeoPts rest)

mkLinearRing :: (Double, Double) -> (Double, Double) -> (Double, Double) -> [(Double, Double)] -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
mkLinearRing p1 p2 p3 rest = LinearRing.makeLinearRing (tupleToGeoPts p1) (tupleToGeoPts p2) (tupleToGeoPts p3) (Sequence.fromList $ fmap tupleToGeoPts rest)

tupleToGeoPts :: (Double, Double) -> Geospatial.GeoPositionWithoutCRS
tupleToGeoPts (x, y) = Geospatial.GeoPointXY (Geospatial.PointXY x y)

listToVectorGeo :: [(Double, Double)] -> Sequence.Seq Geospatial.PointXY
listToVectorGeo pts = Sequence.fromList $ fmap (uncurry Geospatial.PointXY) pts

listToSequenceGeo :: [(Double, Double)] -> Sequence.Seq Geospatial.PointXY
listToSequenceGeo pts = Sequence.fromList $ fmap (uncurry Geospatial.PointXY) pts
