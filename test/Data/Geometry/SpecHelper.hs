module Data.Geometry.SpecHelper where

import qualified Data.Geospatial      as Geospatial
import qualified Data.LineString      as LineString
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Geography.VectorTile as VectorTile

tupleToPts :: [(Int, Int)] -> VectorStorable.Vector VectorTile.Point
tupleToPts = foldr (\(x,y) acc -> VectorTile.Point x y `VectorStorable.cons` acc) VectorStorable.empty

mkLineString :: (Double, Double) -> (Double, Double) -> [(Double, Double)] -> LineString.LineString Geospatial.GeoPositionWithoutCRS
mkLineString p1 p2 rest = LineString.makeLineString (tupleToGeoPts p1) (tupleToGeoPts p2) (fmap tupleToGeoPts rest)

tupleToGeoPts :: (Double, Double) -> Geospatial.GeoPositionWithoutCRS
tupleToGeoPts (x, y) = Geospatial.GeoPointXY (Geospatial.PointXY x y)
