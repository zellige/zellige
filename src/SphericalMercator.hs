module SphericalMercator where

-- Remove these
mvtExtents = 2048
mvtX = 28999
mvtY = 19781
zoom = 15

wgs84MajorRadius :: Double
wgs84MajorRadius = 6378137.0

maxExtents :: Double
maxExtents = 20037508.342789244 :: Double

degreesToRadians :: Double -> Double
degreesToRadians x = x / 180 * pi

latLonToXY :: (Double, Double) -> (Int, Int)
latLonToXY (lat, lon) = (x, y)
    where
      x = round ((absLonToX lat - minX) * mvtExtents / spanX)
      y = round ((absLatToY lon - minY) * mvtExtents / spanY)
      spanX = maxX - minX
      spanY = maxY - minY
      (minX, minY, maxX, maxY) = boundingBox mvtX mvtY zoom

absLonToX :: Double -> Double
absLonToX x = wgs84MajorRadius * degreesToRadians x

absLatToY :: Double -> Double
absLatToY y = wgs84MajorRadius * log(tan((pi*0.25) + (0.5 * degreesToRadians y)))

boundingBox :: Integer -> Integer -> Integer -> (Double, Double, Double, Double)
boundingBox x y z = (minX, minY, maxX, maxY)
    where
      minX = -maxExtents + fromIntegral x * resolution
      minY = maxExtents - fromIntegral y * resolution
      maxX = -maxExtents + (fromIntegral x * resolution) + resolution
      maxY = maxExtents - (fromIntegral y * resolution) - resolution
      resolution = maxExtents * 2 / (2.0 ** fromIntegral z)
