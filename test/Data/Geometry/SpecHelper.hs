module Data.Geometry.SpecHelper where

import qualified Data.Geospatial      as Geospatial
import qualified Data.LinearRing      as LinearRing
import qualified Data.LineString      as LineString
import qualified Data.Vector.Storable as VectorStorable
import qualified Geography.VectorTile as VectorTile

tupleToPts :: [(Int, Int)] -> VectorStorable.Vector VectorTile.Point
tupleToPts = foldr (\(x,y) acc -> VectorTile.Point x y `VectorStorable.cons` acc) VectorStorable.empty

mkLineString :: (Double, Double) -> (Double, Double) -> [(Double, Double)] -> LineString.LineString Geospatial.GeoPositionWithoutCRS
mkLineString p1 p2 rest = LineString.makeLineString (tupleToGeoPts p1) (tupleToGeoPts p2) (VectorStorable.fromList $ fmap tupleToGeoPts rest)

mkLinearRing :: (Double, Double) -> (Double, Double) -> (Double, Double) -> [(Double, Double)] -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
mkLinearRing p1 p2 p3 rest = LinearRing.makeLinearRing (tupleToGeoPts p1) (tupleToGeoPts p2) (tupleToGeoPts p3) (VectorStorable.fromList $ fmap tupleToGeoPts rest)

tupleToGeoPts :: (Double, Double) -> Geospatial.GeoPositionWithoutCRS
tupleToGeoPts (x, y) = Geospatial.GeoPointXY (Geospatial.PointXY x y)

listToVectorGeo :: [(Double, Double)] -> VectorStorable.Vector Geospatial.PointXY
listToVectorGeo pts = VectorStorable.fromList $ fmap (uncurry Geospatial.PointXY) pts
