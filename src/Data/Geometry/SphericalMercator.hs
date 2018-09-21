module Data.Geometry.SphericalMercator where

import qualified Data.Aeson                    as Aeson
import qualified Data.Foldable                 as Foldable
import qualified Data.Geospatial               as Geospatial
import qualified Data.LinearRing               as LinearRing
import qualified Data.LineString               as LineString
import qualified Data.Vector                   as Vector

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
    Geospatial.Point g        -> Vector.cons (Geospatial.reWrapGeometry feature $ convertPoint extents qt bb g) acc
    Geospatial.MultiPoint g   -> Vector.cons (Geospatial.reWrapGeometry feature $ convertPoints extents qt bb g) acc
    Geospatial.Line g         -> Vector.cons (Geospatial.reWrapGeometry feature $ convertLine extents qt bb g) acc
    Geospatial.MultiLine g    -> Vector.cons (Geospatial.reWrapGeometry feature $ convertLines extents qt bb g) acc
    Geospatial.Polygon g      -> Vector.cons (Geospatial.reWrapGeometry feature $ convertPolygon extents qt bb g) acc
    Geospatial.MultiPolygon g -> Vector.cons (Geospatial.reWrapGeometry feature $ convertMultiPolygon extents qt bb g) acc
    Geospatial.Collection gs  -> Vector.cons (Geospatial.reWrapGeometry feature $ Geospatial.Collection (Foldable.foldMap (\g -> convertFeature' extents qt bb g Vector.empty) gs)) acc

convertFeature' :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeospatialGeometry -> Vector.Vector Geospatial.GeospatialGeometry -> Vector.Vector Geospatial.GeospatialGeometry
convertFeature' extents qt bb geometry acc =
  case geometry of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> Vector.cons (convertPoint extents qt bb g) acc
    Geospatial.MultiPoint g   -> Vector.cons (convertPoints extents qt bb g) acc
    Geospatial.Line g         -> Vector.cons (convertLine extents qt bb g) acc
    Geospatial.MultiLine g    -> Vector.cons (convertLines extents qt bb g) acc
    Geospatial.Polygon g      -> Vector.cons (convertPolygon extents qt bb g) acc
    Geospatial.MultiPolygon g -> Vector.cons (convertMultiPolygon extents qt bb g) acc
    Geospatial.Collection gs  -> Vector.cons (convertCollection extents qt bb gs) acc

convertPoint :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoPoint -> Geospatial.GeospatialGeometry
convertPoint extents qt bb (Geospatial.GeoPoint point) = Geospatial.Point (Geospatial.GeoPoint newPoint)
  where
    newPoint = latLonToXYInTile extents qt bb point

convertPoints :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoMultiPoint -> Geospatial.GeospatialGeometry
convertPoints extents qt bb (Geospatial.GeoMultiPoint points) = Geospatial.MultiPoint (Geospatial.GeoMultiPoint newPoints)
  where
    newPoints = fmap (latLonToXYInTile extents qt bb) points

convertLine :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeospatialGeometry
convertLine extents qt bb (Geospatial.GeoLine line) = Geospatial.Line (Geospatial.GeoLine newLine)
  where
    newLine = LineString.map (latLonToXYInTile extents qt bb) line

convertLines :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeospatialGeometry
convertLines extents qt bb (Geospatial.GeoMultiLine mLines) = Geospatial.MultiLine (Geospatial.GeoMultiLine newLines)
  where
    newLines = (fmap . LineString.map) (latLonToXYInTile extents qt bb) mLines

convertPolygon :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeospatialGeometry
convertPolygon extents qt bb (Geospatial.GeoPolygon poly) = Geospatial.Polygon (Geospatial.GeoPolygon newPoly)
  where
    newPoly = (fmap . LinearRing.map) (latLonToXYInTile extents qt bb) poly

convertMultiPolygon :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeospatialGeometry
convertMultiPolygon extents qt bb (Geospatial.GeoMultiPolygon polys) = Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon newPolys)
  where
    newPolys = (fmap . fmap . LinearRing.map) (latLonToXYInTile extents qt bb) polys

convertCollection :: Int -> Int -> TypesGeography.BoundingBox -> Vector.Vector Geospatial.GeospatialGeometry -> Geospatial.GeospatialGeometry
convertCollection extents qt bb gs = Geospatial.Collection newCollection
  where
    newCollection = Foldable.foldMap (\g -> convertFeature' extents qt bb g Vector.empty) gs

latLonToXYInTile :: Int -> Int -> TypesGeography.BoundingBox -> Geospatial.GeoPositionWithoutCRS -> Geospatial.GeoPositionWithoutCRS
latLonToXYInTile extents quantizePixels (TypesGeography.BoundingBox minX minY maxX maxY) pt = xy
    where
      xy = if quantizePixels > 1 then newPoint (newQuantize dQuantizePixels x) (newQuantize dQuantizePixels y) else newPoint x y
      newPoint newX newY = Geospatial.GeoPointXY (Geospatial.PointXY newX newY)
      x = (lonToX lat - minX) * dExtents / spanX
      y = (latToY lon - minY) * dExtents / spanY
      (Geospatial.PointXY lat lon) = Geospatial.retrieveXY pt
      dExtents = fromIntegral extents
      dQuantizePixels = fromIntegral quantizePixels
      spanX = maxX - minX
      spanY = maxY - minY

newQuantize :: Double -> Double -> Double
newQuantize pixels i = (i / pixels) * pixels

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
