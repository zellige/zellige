module Data.Geometry.SphericalMercator where

import qualified Data.Aeson                    as Aeson
import qualified Data.Geospatial               as Geospatial
import qualified Data.Vector                   as Vector
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

wgs84MajorRadius :: Double
wgs84MajorRadius = 6378137.0

maxExtents :: Double
maxExtents = 20037508.342789244 :: Double

degreesToRadians :: Double -> Double
degreesToRadians x = x / 180 * pi

convertFeatures :: Int -> Int -> TypesGeography.BoundingBox -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
convertFeatures extents qt bb = Vector.foldr (\x acc -> convertFeature extents qt bb (Geospatial._geometry x) x acc) Vector.empty

convertFeature :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeospatialGeometry -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
convertFeature extents qt bb geometry feature acc =
  case geometry of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> convertPoint extents qt bb g feature acc
    Geospatial.MultiPoint _   -> acc
    Geospatial.Line g         -> convertLine extents qt bb g feature acc
    Geospatial.MultiLine _    -> acc
    Geospatial.Polygon _      -> acc
    Geospatial.MultiPolygon _ -> acc
    Geospatial.Collection _   -> acc

convertPoint :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
convertPoint extents qt bb (Geospatial.GeoPoint point) (Geospatial.GeoFeature bbox _ props fId) = Vector.cons reMakeFeature
    where
      newPoint = newLatLonToXYInTile extents qt bb point
      reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.Point (Geospatial.GeoPoint newPoint)) props fId

convertLine :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
convertLine extents qt bb (Geospatial.GeoLine line) (Geospatial.GeoFeature bbox _ props fId) = Vector.cons reMakeFeature
    where
      newLine = fmap (newLatLonToXYInTile extents qt bb) line
      reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine newLine)) props fId

newLatLonToXYInTile :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoPositionWithoutCRS -> Geospatial.GeoPositionWithoutCRS
newLatLonToXYInTile extents quantizePixels (TypesGeography.BoundingBox minX minY maxX maxY) pt = xy
    where
      xy = if quantizePixels > 1 then newPoint (newQuantize dQuantizePixels x) (newQuantize dQuantizePixels y) else newPoint (toNice x) (toNice y)
      newPoint newX newY = Geospatial.GeoPointXY (Geospatial.PointXY newX newY)
      x = (lonToX lat - minX) * dExtents / spanX
      y = (latToY lon - minY) * dExtents / spanY
      (Geospatial.PointXY lat lon) = Geospatial.retrieveXY pt
      dExtents = fromIntegral extents
      dQuantizePixels = fromIntegral quantizePixels
      spanX = maxX - minX
      spanY = maxY - minY

toNice :: Double -> Double
toNice = (fromIntegral :: Integer -> Double) . round

newQuantize :: Double -> Double -> Double
newQuantize pixels i = toNice $ (i / pixels) * pixels

latLonToXYInTile :: Int -> Int -> TypesGeography.BoundingBox -> TypesGeography.LatLon -> VectorTile.Point
latLonToXYInTile extents quantizePixels (TypesGeography.BoundingBox minX minY maxX maxY) (TypesGeography.LatLon lat lon) = xy
    where
      xy = if quantizePixels > 1 then VectorTile.Point (quantize quantizePixels x) (quantize quantizePixels y) else VectorTile.Point x y
      x = round ((lonToX lat - minX) * dExtents / spanX)
      y = round ((latToY lon - minY) * dExtents / spanY)
      dExtents = fromIntegral extents
      spanX = maxX - minX
      spanY = maxY - minY

quantize :: Int -> Int -> Int
quantize pixels i = (i `quot` pixels) * pixels

-- Longitude 4326 to 3857 X
lonToX :: Double -> Double
lonToX x = checkX tmpX
      where
        tmpX = wgs84MajorRadius * degreesToRadians x
        checkX x' = if x' > maxExtents then maxExtents else x'

-- Latitude 4326 to 3857 Y
latToY :: Double -> Double
latToY y = checkY tmpY
      where
        tmpY = wgs84MajorRadius * log(tan((pi * 0.25) + (0.5 * degreesToRadians y)))
        checkY y' = if y' < -maxExtents then -maxExtents else y'

-- Bounding box in 3857 based on x y zoom.
boundingBox :: TypesGeography.GoogleTileCoordsInt -> TypesGeography.BoundingBox
boundingBox (TypesGeography.GoogleTileCoordsInt zoom (TypesGeography.CoordsInt x y)) = TypesGeography.BoundingBox minX minY maxX maxY
    where
      minX = -maxExtents + fromIntegral x * resolution
      minY = maxExtents - fromIntegral y * resolution
      maxX = -maxExtents + (fromIntegral x * resolution) + resolution
      maxY = maxExtents - (fromIntegral y * resolution) - resolution
      resolution = maxExtents * 2 / (2.0 ** fromIntegral zoom)
