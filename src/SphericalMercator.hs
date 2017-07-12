module SphericalMercator where

-- Based on https://github.com/terranodo/tegola/blob/master/maths/webmercator/main.go

wgs84MajorRadius :: Double
wgs84MajorRadius = 6378137.0

wgs84MinorRadius :: Double
wgs84MinorRadius = 6356752.3142 :: Double

maxExtents :: Double
maxExtents = 20037508.342789244 :: Double

wgs84Ratio :: Double
wgs84Ratio  = wgs84MinorRadius / wgs84MajorRadius

-- First eccentricity
eccentricity :: Double
eccentricity = sqrt (1.0 - (wgs84Ratio * wgs84Ratio) )

coms :: Double
coms = 0.5 * eccentricity

degreesToRadians :: Double -> Double
degreesToRadians x = x / 180 * pi

lonToX :: Double -> Int
lonToX x = round ((absLonToX x - minX) * 2048 / spanX)
  where
    spanX = maxX - minX
    (minX, _, maxX, _) = boundingBox 28999 19781 15

latToY :: Double -> Int
latToY y = round ((absLatToY y - minY) * 2048 / spanY)
  where
    spanY = maxY - minY
    (_, minY, _, maxY) = boundingBox 28999 19781 15

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

-- 	nx = int64((p.X() - c.tile.Minx) * c.extent / c.xspan)
-- 	ny = int64((p.Y() - c.tile.Miny) * c.extent / c.yspan)
