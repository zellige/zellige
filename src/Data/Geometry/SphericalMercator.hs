module Data.Geometry.SphericalMercator where

import           Data.Geometry.Types

wgs84MajorRadius :: Double
wgs84MajorRadius = 6378137.0

maxExtents :: Double
maxExtents = 20037508.342789244 :: Double

degreesToRadians :: Double -> Double
degreesToRadians x = x / 180 * pi

latLonToXYInTile :: Pixels -> BoundingBox -> LatLon -> (Int, Int)
latLonToXYInTile (Pixels extents) (BoundingBox minX minY maxX maxY) (LatLon lat lon) = (x, y)
    where
      x = round ((lonToX lat - minX) * dExtents / spanX)
      y = round ((latToY lon - minY) * dExtents / spanY)
      dExtents = fromIntegral extents
      spanX = maxX - minX
      spanY = maxY - minY

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
boundingBox :: GoogleTileCoords -> BoundingBox
boundingBox (GoogleTileCoords zoom (Coords x y)) = BoundingBox minX minY maxX maxY
    where
      minX = -maxExtents + fromIntegral x * resolution
      minY = maxExtents - fromIntegral y * resolution
      maxX = -maxExtents + (fromIntegral x * resolution) + resolution
      maxY = maxExtents - (fromIntegral y * resolution) - resolution
      resolution = maxExtents * 2 / (2.0 ** fromIntegral zoom)
